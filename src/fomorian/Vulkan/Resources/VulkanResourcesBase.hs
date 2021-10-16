{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
import Data.Vector as V hiding (mapM_)

import Foreign.Storable (Storable, sizeOf)
import Foreign.Marshal.Array
import Foreign.Ptr (nullPtr, plusPtr,castPtr)
import System.FilePath

import Linear

import LoadUnload
import AsyncLoader

import Fomorian.SceneNode
import Fomorian.NeutralSceneTarget
import Fomorian.SceneResources
import Fomorian.SimpleMemoryArena
import Fomorian.GraphicsLoaders.ProcessWavefront (OBJBufferRecord, loadWavefrontOBJFile)

import Fomorian.Vulkan.WindowBundle
import Fomorian.Vulkan.Resources.DeviceMemoryTypes (AbstractMemoryType(..))
import Fomorian.Vulkan.Resources.DeviceMemoryAllocator

import Vulkan.Core10 (Buffer, Device, MemoryRequirements(..), BufferUsageFlags, BufferCreateInfo(..), DeviceSize, Image, ImageView)
import qualified Vulkan.Core10 as VK
import Vulkan.Zero as VZ

type VulkanDataSourceTypes = BasicDataSourceTypes
  .+ ("placeholderSource" .== ())
  
type VulkanResourceTypes =
     ("vkGeometry" .==  GeometryResource VBuffer IBuffer String)
  .+ ("placeholderResource" .== Int)
  .+ ("textureImage" .== ColorBuffer)

data VBuffer = VBuffer Buffer (MemoryAllocation DeviceSize) deriving (Eq, Show)
data IBuffer = IBuffer Buffer (MemoryAllocation DeviceSize) deriving (Eq, Show)
data UBuffer = UBuffer Buffer (MemoryAllocation DeviceSize) deriving (Eq, Show)


data ColorBuffer = ColorBuffer Image (MemoryAllocation DeviceSize) ImageView deriving (Eq, Show)
data DepthBuffer = DepthBuffer Image (MemoryAllocation DeviceSize) ImageView deriving (Eq, Show)

