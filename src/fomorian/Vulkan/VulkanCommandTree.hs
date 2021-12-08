{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Fomorian.Vulkan.VulkanCommandTree where

import Control.Monad.Freer

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Data.Row
import Data.Row.Records as R
import Data.Functor.Foldable
import Data.HList

import Linear

import Foreign.Storable

import qualified Vulkan.Core10 as VK

import Fomorian.SceneNode
import Fomorian.SceneResources
import Fomorian.StorableLayout

import Fomorian.Vulkan.VulkanMonads
import Fomorian.Vulkan.Resources.VulkanResourcesBase
import Fomorian.Vulkan.Resources.DescriptorSetHelper
    ( writeToHelperBundle,
      nextDescriptorSetBundle,
      useDescriptorSetHelperSource )
import Fomorian.Vulkan.VulkanTargetTree



data VulkanCommand

type instance (InvokeReq VulkanCommand ir) = (HasType "pipe" PipelineBundle ir,
                                              HasType "renderPass" VK.RenderPass ir,
                                              HasType "vertexData" (GeometryResource VBuffer IxBuffer DataLayoutMap) ir,
                                              HasType "imageBuffer" ImageBuffer ir,
                                              HasType "descriptorSetHelper" DescriptorSetHelperSource ir)

type instance (DrawReq VulkanCommand dr) = (HasType "modelMatrix" (M44 Float) dr,
                                               HasType "viewMatrix" (M44 Float) dr,
                                               HasType "projectionMatrix" (M44 Float) dr)


-- | Given an OpenGLTarget scene graph and loaded resources, builds an VulkanCommand scene graph which can be directly
--   converted into a monad to draw the scene
vulkanCommandAlgebra :: VulkanResources -> (VK.Format, VK.Format) -> SceneGraphF VulkanTargetTree dr (SceneGraph VulkanCommand dr) -> SceneGraph VulkanCommand dr
vulkanCommandAlgebra (VulkanResources h) fmtz (InvokeF x) =
  let vSource    = (x .! #vkVertex) ::VulkanDataSource
      txSource   = (x .! #vkImage) :: VulkanDataSource
      basicDescriptorInfo = DescriptorSetInfo [
              UniformDescriptor 0 VK.SHADER_STAGE_VERTEX_BIT (fromIntegral $ sizeOf @DefaultMatrices undefined) (fromIntegral $ Foreign.Storable.alignment @DefaultMatrices undefined),
              CombinedDescriptor 1 VK.SHADER_STAGE_FRAGMENT_BIT 1 V.empty
            ]
      basicDescriptorSource = (DataSource $ IsJust #descriptorHelperSettings basicDescriptorInfo :: VulkanDataSource)
      basicRenderpassSource = DataSource $ IsJust #renderPassFormat fmtz
      basicPipelineSource   = DataSource $ IsJust #pipelineSettings $ SimplePipelineSettings {
              renderPassFormat = fmtz,
              shaderSource = (x .! #pipeShader) :: FilePath,
              descriptorSetLayouts = [basicDescriptorInfo]
            }
      (Just vertices)       = pullResource h vSource #vkGeometry
      (Just imagez)         = pullResource h txSource #textureImage
      (Just dSetHelper)     = pullResource h basicDescriptorSource #descriptorSetHelperSource
      (Just rPass)          = pullResource h basicRenderpassSource #renderPass
      (Just pBundle)        = (pullResource h basicPipelineSource #simplePipeline :: Maybe PipelineBundle)
  in
    Invoke $   #pipe             .== pBundle
            .+ #renderPass       .== rPass
            .+ #vertexData       .== vertices
            .+ #imageBuffer      .== imagez
            .+ #descriptorSetHelper .== dSetHelper
vulkanCommandAlgebra _ _    (GroupF cmds)       = Group cmds
vulkanCommandAlgebra r fmtz (TransformerF t gr) = Transformer t (vulkanToCommand r fmtz gr)


vulkanToCommand :: VulkanResources -> (VK.Format, VK.Format) -> SceneGraph VulkanTargetTree dr -> SceneGraph VulkanCommand dr
vulkanToCommand r fmtz = cata (vulkanCommandAlgebra r fmtz)


newtype VkDrawCmd dr m a = VDC { runVDC :: Rec dr -> Int -> VK.CommandBuffer -> m a }

noopDraw :: (Monad m) => a -> VkDrawCmd dr m a
noopDraw x = return x

instance (Functor m) => Functor (VkDrawCmd dr m) where
  fmap f dc = VDC $ \r s c -> fmap f (runVDC dc r s c)

instance (Applicative m) => Applicative (VkDrawCmd dr m) where
  pure x               = VDC $ \_ _ _ -> pure x
  (VDC fa) <*> (VDC b) = VDC $ \r s c -> (fa r s c) <*> (b r s c)

instance (Monad m) => Monad (VkDrawCmd dr m) where
  return     = pure
  a >>= b    = VDC $ \r  s c-> do a' <- runVDC a r s c
                                  runVDC (b a') r s c


-- | Drawing a single thing with OpenGL - shader parameters must be Uniform-valid, which
--   means they are GLfloat, GLint, or vectors (V2,V3,V4) or matrices.
invokeVulkan :: (InvokeReq VulkanCommand ir, DrawReq VulkanCommand dr, 
                 InVulkanMonad effs) => Rec ir -> VkDrawCmd dr (Eff effs) ()
invokeVulkan r = VDC $ \dr slotIndex cBuf ->
  do
    -- statics from the 'Invoke' node
    let dData = r .! #descriptorSetHelper
        imagez = r .! #imageBuffer
        vertices = r .! #vertexData
        (PipelineBundle pLayout pipe) = r .! #pipe

    curDSet <- useDescriptorSetHelperSource dData slotIndex $ do
      dSetBundle <- nextDescriptorSetBundle
      let (ImageBuffer _ _ _ iv samp) = imagez
          flipProjection = scaled (V4 1 (-1) 1 1)
          uBuf = DefaultMatrices {
            -- GLSL requires tranlation values are in 13,14,15th element of the matrix layout in
            -- memory. This is not how matrices in Linear are laid out, so we need to transpose.
            -- We don't need this in the OpenGL renderer since 'setUniform' of uses GL_TRANSPOSE set to true.
            matrModel = transpose $ dr .! #modelMatrix,
            matrView = transpose $ dr .! #viewMatrix,
            matrProjection = transpose $ dr .! #projectionMatrix
        }
      writeToHelperBundle dSetBundle $ uBuf `HCons` (iv,samp) `HCons` HNil
      return $ dSetHandle dSetBundle

    VK.cmdBindPipeline cBuf VK.PIPELINE_BIND_POINT_GRAPHICS pipe
    
    let (GeometryResource (VBuffer vBuf _) (Just (IxBuffer ixBuf _)) elements _) = vertices
    VK.cmdBindVertexBuffers cBuf 0 (V.singleton vBuf) (V.singleton 0)
    VK.cmdBindIndexBuffer cBuf ixBuf 0 VK.INDEX_TYPE_UINT32

    VK.cmdBindDescriptorSets cBuf VK.PIPELINE_BIND_POINT_GRAPHICS pLayout 0 (V.singleton curDSet) V.empty
    VK.cmdDrawIndexed cBuf (fromIntegral elements) 1 0 0 0

vulkanGoAlgebra :: (InVulkanMonad effs) => SceneGraphF VulkanCommand dr (VkDrawCmd dr (Eff effs) ()) -> VkDrawCmd dr (Eff effs) ()
vulkanGoAlgebra (InvokeF x)     = invokeVulkan x
vulkanGoAlgebra (GroupF cmds)   = foldl (>>) (VDC $ \_ _ _-> return ()) cmds
vulkanGoAlgebra (TransformerF t gr) = VDC $ \fd s c ->
         let fd2  = t fd
             scmd = cata vulkanGoAlgebra gr
             in runVDC scmd fd2 s c


vulkanGo :: (InVulkanMonad effs) => SceneGraph VulkanCommand dr -> Rec dr -> Int -> VK.CommandBuffer -> Eff effs ()
vulkanGo sg fd s c = let sm = cata vulkanGoAlgebra sg
                     in runVDC sm fd s c



-- | Compile a VulkanCommand tree into a tree where draw calls are grouped/binned by pipeline and renderpass.
--  The first grouping is by 'RenderPass' and then the second grouping is by Pipeline.
data InvocationTrie dr effs = InvocationTrie (M.Map VK.RenderPass (M.Map PipelineBundle (VkDrawCmd dr (Eff effs) ()))) --(S.Set DescriptorSetHelper)

instance Semigroup (InvocationTrie dr effs) where
  InvocationTrie m1 <> InvocationTrie m2 = InvocationTrie $ M.unionWith (M.unionWith (>>)) m1 m2

instance Monoid (InvocationTrie dr effs) where
  mempty = InvocationTrie M.empty

-- | Drawing a single thing with OpenGL - shader parameters must be Uniform-valid, which
--   means they are GLfloat, GLint, or vectors (V2,V3,V4) or matrices.
invokeVulkanNoPipeBind :: (InvokeReq VulkanCommand ir, DrawReq VulkanCommand dr, 
                 InVulkanMonad effs) => Rec ir -> VkDrawCmd dr (Eff effs) ()
invokeVulkanNoPipeBind r = VDC $ \dr slotIndex cBuf ->
  do
    -- statics from the 'Invoke' node
    let dData = r .! #descriptorSetHelper
        imagez = r .! #imageBuffer
        vertices = r .! #vertexData
        (PipelineBundle pLayout pipe) = r .! #pipe

    curDSet <- useDescriptorSetHelperSource dData slotIndex $ do
      dSetBundle <- nextDescriptorSetBundle
      let (ImageBuffer _ _ _ iv samp) = imagez
          flipProjection = scaled (V4 1 (-1) 1 1)
          uBuf = DefaultMatrices {
            -- GLSL requires tranlation values are in 13,14,15th element of the matrix layout in
            -- memory. This is not how matrices in Linear are laid out, so we need to transpose.
            -- We don't need this in the OpenGL renderer since 'setUniform' of uses GL_TRANSPOSE set to true.
            matrModel = transpose $ dr .! #modelMatrix,
            matrView = transpose $ dr .! #viewMatrix,
            matrProjection = transpose $ dr .! #projectionMatrix
        }
      writeToHelperBundle dSetBundle $ uBuf `HCons` (iv,samp) `HCons` HNil
      return $ dSetHandle dSetBundle

    let (GeometryResource (VBuffer vBuf _) (Just (IxBuffer ixBuf _)) elements _) = vertices
    VK.cmdBindVertexBuffers cBuf 0 (V.singleton vBuf) (V.singleton 0)
    VK.cmdBindIndexBuffer cBuf ixBuf 0 VK.INDEX_TYPE_UINT32

    VK.cmdBindDescriptorSets cBuf VK.PIPELINE_BIND_POINT_GRAPHICS pLayout 0 (V.singleton curDSet) V.empty
    VK.cmdDrawIndexed cBuf (fromIntegral elements) 1 0 0 0



transformTrie :: (Rec dr2 -> Rec dr) -> InvocationTrie dr effs -> InvocationTrie dr2 effs
transformTrie t (InvocationTrie m) = InvocationTrie $ M.map (M.map transformCmd) m
  where
    --transformCmd :: VkDrawCmd dr (Eff effs) () -> VkDrawCmd dr2 (Eff effs) ()
    transformCmd cmd = VDC $ \fd s c ->
      let fd2 = t fd
      in runVDC cmd fd2 s c

instance Show (InvocationTrie dr effs) where
  show (InvocationTrie m) = M.foldrWithKey showRPass "" m
    where
      showRPass kr r accum = ("Renderpass=" ++ show kr ++ ": " ++ M.foldrWithKey showPipes "" r)
      showPipes kp p accum = ("(Pipe=" ++ show kp ++ ")")

vulkanCommandToTrie :: (InVulkanMonad effs) => SceneGraphF VulkanCommand dr (InvocationTrie dr effs) -> InvocationTrie dr effs
vulkanCommandToTrie (InvokeF x) =
  let rPass    = x .! #renderPass
      pBundle = x .! #pipe
  in
      InvocationTrie (M.singleton rPass (M.singleton pBundle (invokeVulkanNoPipeBind x)))
vulkanCommandToTrie (GroupF cmds) = foldl (<>) mempty cmds
vulkanCommandToTrie (TransformerF t gr) =
  -- an InvocationTrie using dr2 instead of dr
  let invocationTrie2 = cata vulkanCommandToTrie gr
  in transformTrie t invocationTrie2

compileToInvocationTrie :: (InVulkanMonad effs) => SceneGraph VulkanCommand dr -> InvocationTrie dr effs
compileToInvocationTrie = cata vulkanCommandToTrie

runInvocationTrie :: (InVulkanMonad effs) => InvocationTrie dr effs -> Rec dr -> Int -> VK.CommandBuffer -> VK.Framebuffer -> VK.Extent2D -> Eff effs ()
runInvocationTrie (InvocationTrie trie) frameData s cBuf attachments ext2d@(VK.Extent2D w h) = M.foldrWithKey runTrieRenderPass (return ()) trie
  where
    runTrieRenderPass rPass pipesMap cmdAccum = cmdAccum >> do
      -- start/end the render pass and run the subtree pipelines in the middle
      let renderarea = VK.Rect2D (VK.Offset2D 0 0) ext2d
      let clearTo = V.fromList [VK.Color (VK.Float32 0 0.5 0.5 1), VK.DepthStencil (VK.ClearDepthStencilValue 1.0 0)]
      VK.cmdBeginRenderPass cBuf (VK.RenderPassBeginInfo () rPass attachments renderarea clearTo) VK.SUBPASS_CONTENTS_INLINE

      M.foldrWithKey runTriePipeline (return ()) pipesMap

      VK.cmdEndRenderPass cBuf

    runTriePipeline (PipelineBundle pLayout pipe) pipeCmd cmdAccum = cmdAccum >> do
      VK.cmdBindPipeline cBuf VK.PIPELINE_BIND_POINT_GRAPHICS pipe
      runVDC pipeCmd frameData s cBuf
    




