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



import Data.Proxy
import Data.Row
import Data.Functor.Foldable
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Vector as V hiding (mapM_,(++))
import Data.HList.HList

import Foreign.Storable
import Foreign.Ptr (castPtr)
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

import qualified Vulkan.Core10 as VK

--
-- Convert into a vulkan target tree and compute needed resources
--

data VulkanTargetTree

type instance (InvokeReq VulkanTargetTree ir) = (HasType "pipeShader" FilePath ir,
                                                 HasType "vkImage" VulkanDataSource ir,
                                                 HasType "vkVertex" VulkanDataSource ir)

type instance (DrawReq VulkanTargetTree dr) = (HasType "modelMatrix" (M44 Float) dr,
                                               HasType "viewMatrix" (M44 Float) dr,
                                               HasType "projectionMatrix" (M44 Float) dr)

neutralToVulkanTargetAlg ::SceneGraphF NeutralSceneTarget dr (SceneGraph VulkanTargetTree dr) -> SceneGraph VulkanTargetTree dr
neutralToVulkanTargetAlg (InvokeF x) = Invoke $
                (#pipeShader .== x .! #shader)
             .+ (#vkVertex   .== diversifyToVulkanSource (x .! #geometry))
             .+ (#vkImage   .== diversifyToVulkanSource (Prelude.head (x .! #textures)))
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
vulkanResourcesAlgebra fmtz (TransformerF _ gr) = vulkanResourcesScene fmtz gr
vulkanResourcesAlgebra fmtz (InvokeF x) =
  let v    = x .! #vkVertex
      tx  = x .! #vkImage
      basicDescriptorInfo = DescriptorSetInfo [
              UniformDescriptor 0 VK.SHADER_STAGE_VERTEX_BIT (fromIntegral $ sizeOf @DefaultMatrices undefined) (fromIntegral $ Foreign.Storable.alignment @DefaultMatrices undefined),
              CombinedDescriptor 1 VK.SHADER_STAGE_FRAGMENT_BIT 1 V.empty
            ]
      descriptorSetHelperSource = (DataSource $ IsJust #descriptorHelperSettings basicDescriptorInfo :: VulkanDataSource)
      pipelineSource   = DataSource $ IsJust #pipelineSettings $ SimplePipelineSettings {
              renderPassFormat = fmtz,
              shaderSource = x .! #pipeShader,
              descriptorSetLayouts = [basicDescriptorInfo]
            }
  in
    VulkanDataSources $ S.fromList [v, descriptorSetHelperSource, pipelineSource, tx]

vulkanResourcesScene :: (VK.Format, VK.Format) -> SceneGraph VulkanTargetTree dr -> VulkanDataSources
vulkanResourcesScene fmtz = cata (vulkanResourcesAlgebra fmtz)

