{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Fomorian.Vulkan.Resources.VulkanResourcesBase where

import GHC.Generics

import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Concurrent
import Control.Concurrent.Async.Pool
import Control.Exception
import Control.Concurrent.STM

import Control.Monad.Freer
import Control.Monad.Freer.Reader (Reader(..), ask, runReader)

import Data.Maybe (isJust)
import Data.Word
import Data.Row
import Data.Row.Variants (view)
import Data.Functor.Foldable
import Data.Foldable (find)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Vector as V hiding (mapM_,(++))

import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr (Ptr, nullPtr, plusPtr, castPtr)
import System.FilePath

import Linear

import STMLoader.LoadUnload
import STMLoader.AsyncLoader

import Fomorian.SceneNode
import Fomorian.NeutralSceneTarget
import Fomorian.SceneResources
import Fomorian.StorableLayout
import Fomorian.SimpleMemoryArena
import Fomorian.GraphicsLoaders.ProcessWavefront (OBJBufferRecord, loadWavefrontOBJFile)

import Fomorian.Vulkan.WindowBundle
import Fomorian.Vulkan.Resources.DeviceMemoryTypes (AbstractMemoryType(..))
import Fomorian.Vulkan.Resources.DeviceMemoryAllocator

import Vulkan.Core10 (Buffer, Device, BufferUsageFlags, BufferCreateInfo(..), DeviceSize, DeviceMemory, Format, Image, ImageView, Sampler, ShaderModule, RenderPass)
import qualified Vulkan.Core10 as VK

type VulkanDataSourceTypes = BasicDataSourceTypes
  .+ ("renderPassFormat"  .== (Format,Format))    -- ^ produces a renderPass
  .+ ("pipelineSettings"  .== PipelineSettings)   -- ^ produces a simplePipeline
  .+ ("descriptorLayoutInfo" .== DescriptorSetInfo)  -- ^ produces a descriptorSetLayout
  .+ ("descriptorSourceSettings"  .== DescriptorSetInfo)  -- ^ produces a descriptorSetSource
  .+ ("descriptorHelperSettings"  .== DescriptorSetInfo)  -- ^ produces a descriptorSetSource

type VulkanDataSource = DataSource VulkanDataSourceTypes
  
type VulkanResourceTypes =
     ("vkGeometry"     .==  GeometryResource VBuffer IxBuffer DataLayoutMap)
  .+ ("textureImage"   .== ImageBuffer)
  .+ ("shaderModule"   .== ShaderModule)
  .+ ("renderPass"     .== RenderPass)
  .+ ("simplePipeline"      .== PipelineBundle)
  .+ ("descriptorSetLayout" .== VK.DescriptorSetLayout)
  .+ ("descriptorSetSource" .== DescriptorSetSource)
  .+ ("descriptorSetHelperSource" .== DescriptorSetHelperSource)

type VulkanResource = Resource VulkanResourceTypes

-- | Buffer for vertex data
data VBuffer = VBuffer Buffer (MemoryAllocation DeviceSize) deriving (Eq, Show)

-- | Buffer for index data
data IxBuffer = IxBuffer Buffer (MemoryAllocation DeviceSize) deriving (Eq, Show)

-- | Buffer for image data, including the imageview and sampler
data ImageBuffer = ImageBuffer Image Int (MemoryAllocation DeviceSize) ImageView Sampler deriving (Eq, Show)

-- | Buffer for uniforms
data UBuffer = UBuffer Buffer (MemoryAllocation DeviceSize) deriving (Eq, Show)

-- | A temporary buffer to hold CPU-side data. Typically you create a CPU-accessible
--   staging buffer, copy data into it on the CPU, and then transfer the data to a
--   GPU-local buffer so the GPU can access it faster.
data StagingBuffer = StagingBuffer Buffer (MemoryAllocation DeviceSize) deriving (Eq, Show)


data ColorBuffer = ColorBuffer Image (MemoryAllocation DeviceSize) ImageView deriving (Eq, Show)
data DepthBuffer = DepthBuffer Image (MemoryAllocation DeviceSize) ImageView deriving (Eq, Show)

data PipelineSettings =
  SimplePipelineSettings
  {
    renderPassFormat :: (Format,Format),
    shaderSource :: FilePath,
    descriptorSetLayouts :: [DescriptorSetInfo]
  }
  deriving (Eq, Ord, Show)

data PipelineBundle = PipelineBundle VK.PipelineLayout VK.Pipeline
  deriving (Eq, Ord, Show)

data DescriptorBinding where
  --                   bindIndex stages                 sizeOF           alignment
  UniformDescriptor :: Word32 -> VK.ShaderStageFlags -> VK.DeviceSize -> VK.DeviceSize -> DescriptorBinding
  --                    bindIndex stages                 count      immutable samplers
  CombinedDescriptor :: Word32 -> VK.ShaderStageFlags -> Word32 -> (Vector Sampler) -> DescriptorBinding
    deriving (Eq,Ord,Show)


descriptorBindingToVulkan :: DescriptorBinding -> VK.DescriptorSetLayoutBinding
descriptorBindingToVulkan (UniformDescriptor bindIndex stages _ _)       = VK.DescriptorSetLayoutBinding bindIndex VK.DESCRIPTOR_TYPE_UNIFORM_BUFFER 1 stages V.empty
descriptorBindingToVulkan (CombinedDescriptor bindIndex stages count im) = VK.DescriptorSetLayoutBinding bindIndex VK.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER count stages im

newtype DescriptorSetInfo = DescriptorSetInfo [DescriptorBinding]
  deriving (Eq,Ord,Show)

-- | sub pools are in a 'TMVar' 
data DescriptorSetSource = DescriptorSetSource (TMVar (M.Map Int DescriptorSetFixedPool)) DescriptorSetInfo VK.DescriptorSetLayout
  deriving (Eq)

instance Show DescriptorSetSource where
  show (DescriptorSetSource pools info layout) = "DescriptorSetSource (" ++ show info ++ ")"

-- | A wrapper around 'VK.DescriptorPool' which handles a pool of descriptor sets all with the same layout.
data DescriptorSetFixedPool = DescriptorSetFixedPool
  {
    dPoolHandle :: VK.DescriptorPool,
    dSetInfo :: DescriptorSetInfo,
    dSetLayout :: VK.DescriptorSetLayout,
    maxDescriptorSets:: Int,
    freeDescriptorSets :: S.Set VK.DescriptorSet,
    usedDescriptorSets :: S.Set VK.DescriptorSet
  }
  deriving (Show)

--
-- DescriptorSetHelper
--
-- DescriptorSetHelper manages descriptor set by dynamically allocating multiple sub-pools.
-- It also manage the assocatied uniform buffers and memory.
--


data DescriptorSetHelperSource = DescriptorSetHelperSource (TMVar (M.Map Int DescriptorSetHelper)) DescriptorSetInfo VK.DescriptorSetLayout
  deriving (Eq)

data DescriptorSetHelper = DescriptorSetHelper
  {
    freeSubHelpers :: V.Vector DescriptorSetHelperPool,
    usedSubHelpers :: V.Vector DescriptorSetHelperPool,
    dSetInfo :: DescriptorSetInfo,
    dSetLayout :: VK.DescriptorSetLayout,
    subHelperChunkSize :: Int
  }
  deriving (Show)

data DescriptorSetHelperPool = DescriptorSetHelperPool
  {
    freeDescriptorBundles :: Vector DescriptorHelperBundle,
    usedDescriptorBundles :: Vector DescriptorHelperBundle,
    dPoolHandle :: VK.DescriptorPool,
    uniformBuffers :: Vector (Maybe UBuffer)
  }
  deriving (Show)

data DescriptorHelperBundle = DescriptorHelperBundle
  {
    dSetHandle :: VK.DescriptorSet,
    dSetUniformBuffers :: Vector (Maybe (UBuffer, DeviceSize))
  }
  deriving (Show)
