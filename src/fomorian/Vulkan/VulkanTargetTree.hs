{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Fomorian.Vulkan.VulkanTargetTree where

import GHC.Generics

import Control.Monad.IO.Class (liftIO, MonadIO)

import Control.Monad.Freer
import Control.Monad.Freer.Reader (Reader(..), ask, runReader)

import Data.Maybe (isJust)
import Data.Word
import Data.Row
import Data.Row.Variants (view)
import Data.Functor.Foldable
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Vector as V hiding (mapM_,(++))
import Data.HList.HList

import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr (Ptr, nullPtr, plusPtr, castPtr)
import System.FilePath

import Linear

import Fomorian.SceneNode
import Fomorian.NeutralSceneTarget
import Fomorian.SceneResources
import Fomorian.StorableLayout

import Fomorian.Vulkan.WindowBundle
import Fomorian.Vulkan.Resources.DeviceMemoryTypes (AbstractMemoryType(..))
import Fomorian.Vulkan.Resources.DeviceMemoryAllocator
import Fomorian.Vulkan.Resources.VulkanResourcesBase
import Fomorian.Vulkan.Resources.DescriptorSetHelper

import Vulkan.Core10 (Buffer, Device, BufferUsageFlags, BufferCreateInfo(..), DeviceSize, DeviceMemory, Format, Image, ImageView, Sampler, ShaderModule, RenderPass)
import qualified Vulkan.Core10 as VK

--
-- Convert into a vulkan target tree and compute needed resources
--

data VulkanTargetTree

type instance (InvokeReq VulkanTargetTree ir) = (HasType "pipeShader" FilePath ir,
                                                 HasType "geometry" BasicDataSource ir,
                                                 HasType "texture" [BasicDataSource] ir)
type instance (DrawReq VulkanTargetTree dr) = (HasType "modelMatrix" (M44 Float) dr,
                                               HasType "viewMatrix" (M44 Float) dr,
                                               HasType "projectionMatrix" (M44 Float) dr)

neutralToVulkanTargetAlg ::SceneGraphF NeutralSceneTarget dr (SceneGraph VulkanTargetTree dr) -> SceneGraph VulkanTargetTree dr
neutralToVulkanTargetAlg (InvokeF x) = Invoke $
                (#pipeShader .== x .! #shader)
             .+ (#geometry   .== x .! #geometry)
             .+ (#texture    .== x .! #textures)
neutralToVulkanTargetAlg (GroupF xs) = Group xs
neutralToVulkanTargetAlg (TransformerF f gr) = Transformer f (neutralToVulkanTarget gr)

neutralToVulkanTarget :: SceneGraph NeutralSceneTarget dr -> SceneGraph VulkanTargetTree dr
neutralToVulkanTarget = cata neutralToVulkanTargetAlg



newtype VulkanDataSources = VulkanDataSources (S.Set VulkanDataSource)
  deriving (Eq, Show)
  deriving (Monoid,Semigroup) via (S.Set VulkanDataSource)

newtype VulkanResources = VulkanResources (M.Map VulkanDataSource VulkanResource)

data DefaultMatrices = DefaultMatrices
  {
    matrModel :: M44 Float,
    matrView  :: M44 Float,
    matrProjection :: M44 Float
  }
  deriving (Eq,Show,Generic)

instance Storable DefaultMatrices where
  sizeOf _ = sizeOf @(M44 Float) undefined + sizeOf @(M44 Float) undefined + sizeOf @(M44 Float) undefined
  alignment _ = 16 -- maybe only needs to be 4?
  peek p = do
    -- treat as an array of @M44 Float@ objects
    let pMats = castPtr @DefaultMatrices @(M44 Float) p
    m <- peekElemOff pMats 0
    v <- peekElemOff pMats 1
    p <- peekElemOff pMats 2
    return (DefaultMatrices m v p)
  poke p (DefaultMatrices m v prj) = do
    let pMats = castPtr @DefaultMatrices @(M44 Float) p
    pokeElemOff pMats 0 m
    pokeElemOff pMats 1 v
    pokeElemOff pMats 2 prj

type DefaultDescriptorFields = '[DefaultMatrices, ImageSamplerEntry]

helperDescriptorInfo :: DescriptorSetInfo
helperDescriptorInfo = dSetTypeToInfo (Proxy @(HList HelperDescriptorFields)) (hEnd $ hBuild VK.SHADER_STAGE_VERTEX_BIT VK.SHADER_STAGE_FRAGMENT_BIT)


-- | Given a scene node returns the resources used
vulkanResourcesAlgebra :: (VK.Format, VK.Format) -> SceneGraphF VulkanTargetTree dr VulkanDataSources -> VulkanDataSources
vulkanResourcesAlgebra _ (GroupF cmds) = Prelude.foldl (<>) mempty cmds
vulkanResourcesAlgebra _ (TransformerF _ gr) = vulkanResourcesScene gr
vulkanResourcesAlgebra fmtz (InvokeF x) =
  let v    = x .! #geometry
      txs  = x .! #texture
      basicDescriptorInfo = DescriptorSetInfo [
              UniformDescriptor 0 VK.SHADER_STAGE_VERTEX_BIT (fromIntegral $ sizeOf @DefaultMatrices undefined) (fromIntegral $ Foreign.Storable.alignment @DefaultMatrices undefined),
              CombinedDescriptor 1 VK.SHADER_STAGE_FRAGMENT_BIT 1 V.empty
            ]
      basicPipelineSource   = DataSource $ IsJust #pipelineSettings $ SimplePipelineSettings {
              renderPassFormat = fmtz,
              shaderSource = x .! #pipeShader,
              descriptorSetLayouts = [basicDescriptorInfo]
            }
  in
    VulkanDataSources $ S.fromList ([v,basicPipelineSource] ++ txs)
  where
    bumpTex :: BasicDataSource -> VulkanDataSource
    bumpTex (DataSource t) = switch t $
         #userSource     .== DataSource . IsJust #userSource
      .+ #wavefrontPath  .== DataSource . IsJust #wavefrontPath
      .+ #shaderPath     .== DataSource . IsJust #shaderPath
      .+ #texturePath    .== DataSource . IsJust #texturePath

vulkanResourcesScene :: (VK.Format, VK.Format) -> SceneGraph VulkanTargetTree dr -> VulkanDataSources
vulkanResourcesScene fmz = cata (vulkanResourcesAlgebra fmtz)

{-
      let basicVertSource = DataSource $ IsJust #wavefrontPath "testcube.obj"
          basicImageSource = DataSource $ IsJust #texturePath "sad-crab.png"
          basicDescriptorInfo = DescriptorSetInfo [
              UniformDescriptor 0 VK.SHADER_STAGE_VERTEX_BIT (fromIntegral $ sizeOf @HelperExample undefined) (fromIntegral $ Foreign.Storable.alignment @HelperExample undefined),
              CombinedDescriptor 1 VK.SHADER_STAGE_FRAGMENT_BIT 1 V.empty
            ]
          basicDescriptorSource = (DataSource $ IsJust #descriptorHelperSettings basicDescriptorInfo :: VulkanDataSource)
          basicRenderpassFormat = (cFormat,dFormat)
          basicRenderpassSource = DataSource $ IsJust #renderPassFormat basicRenderpassFormat
          basicPipelineSource = DataSource $ IsJust #pipelineSettings $ SimplePipelineSettings {
              renderPassFormat = basicRenderpassFormat,
              shaderSource = "tut",
              descriptorSetLayouts = [basicDescriptorInfo]
            } -}
