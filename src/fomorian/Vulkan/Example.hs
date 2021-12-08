{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Fomorian.Vulkan.Example where

import Control.Exception
import Control.Monad.Freer

import Data.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Row
import Data.HList
import Data.Maybe (fromJust)

import Linear

import Foreign.Storable (sizeOf, alignment)

import STMLoader.AsyncLoader

import Vulkan.Core10 (Extent2D(..), ClearValue(Color, DepthStencil), Offset2D(..), Rect2D(..), ClearColorValue(Float32), ClearDepthStencilValue(..))
import qualified Vulkan.Core10 as VK
import Vulkan.Zero as VZ

import Fomorian.SceneNode
import Fomorian.NeutralSceneTarget
import Fomorian.CommonSceneNodes
import Fomorian.SceneResources
import Fomorian.PlatformRenderer

import qualified Fomorian.Vulkan.WindowBundle as WB

import Fomorian.Vulkan.VulkanMonads
import Fomorian.Vulkan.SwapchainCarousel
import Fomorian.Vulkan.SwapchainBundle
import Fomorian.Vulkan.Resources.DescriptorSetHelper
import Fomorian.Vulkan.Resources.VulkanResourcesBase
import Fomorian.Vulkan.Resources.BoundCommandBuffer
import Fomorian.Vulkan.Resources.VulkanLoader
import Fomorian.Vulkan.VulkanTargetTree
import Fomorian.Vulkan.VulkanCommandTree

testScene3d :: SceneGraph NeutralSceneTarget DefaultDrawFrameParams 
testScene3d = neutral3DSceneRoot $
                perspectiveProject config $
                  autoAspect $ 
                    cameraLookAt (V3 5 10 0) (V3 0 0 0) (V3 0 0 1) $ 
                      group [
                        someCube,
                        translate3d (V3 3 0 0) $ spin3d (V3 0.7071 0.7071 0) 2 someCube,
                        spin3d (V3 0 0.7071 0.7071) 1.7 $ translate3d (V3 3 0 0) $ spin3d (V3 0 1 0) 0.3 $ someCube
                        ]
  where
    config = PerspectiveProject  1.2 {-fov-} 1.0 {-aspect-} 0.1 {-near plane-} 1000 {-far plane-}
    someCube :: (DrawReq NeutralSceneTarget dr) => SceneGraph NeutralSceneTarget dr
    --someCube = wavefrontMesh "tut" "testcube.obj" ["salamander.png"]
    someCube = wavefrontMesh "tut" "testcube.obj" ["salamander.png"]


runSomeVulkan :: IO ()
runSomeVulkan = do
  let cfg = WB.WindowInitConfig "app name" "title" Nothing 1 True
  WB.withWindowBundle cfg $ \wb -> do
    let pd = WB.physicalDeviceHandle (WB.vulkanDeviceBundle wb)
    deviceProps <- VK.getPhysicalDeviceProperties pd
    let deviceLimits = VK.limits deviceProps
    putStrLn $ "buffer-image granularity = " ++ show (VK.bufferImageGranularity deviceLimits)
    -- fork off a queue submit thread using the aux queue
    let aQueue = head $ WB.auxiliaryQueues $ WB.vulkanDeviceBundle wb
    let prebuiltResources = M.empty
    boundQueue <- aQueue `seq` forkBoundSubmitter wb aQueue
    loaderInfo <- startLoader wb boundQueue prebuiltResources

    let curTree = testScene3d

    let runV = runM . runVulkanMonad wb

    finally 
      (runV $ withCarousel 2 $ \swc -> forM_ [1..200] (\ix -> presentNextSlot swc (clearCmd swc loaderInfo curTree ix)))
      (do
         endLoader loaderInfo
         endBoundSubmitter boundQueue
         )
  where
    clearCmd swc resourceLoader sceneGraph curTime cBuf frameBuf cSlot = do
      sb <- sendM $ readIORef (bundleRef swc)
      let (SwapchainPresentInfo cFormat dFormat ext2d@(Extent2D w h)) = swapchainPresentInfo sb

      let basicDescriptorInfo = DescriptorSetInfo [
              UniformDescriptor 0 VK.SHADER_STAGE_VERTEX_BIT (fromIntegral $ sizeOf @HelperExample undefined) (fromIntegral $ Foreign.Storable.alignment @HelperExample undefined),
              CombinedDescriptor 1 VK.SHADER_STAGE_FRAGMENT_BIT 1 V.empty
            ]
          basicDescriptorSource = (DataSource $ IsJust #descriptorHelperSettings basicDescriptorInfo :: VulkanDataSource)
          basicRenderpassFormat = (cFormat,dFormat)
          basicRenderpassSource = DataSource $ IsJust #renderPassFormat basicRenderpassFormat
          targetTree = neutralToVulkanTarget sceneGraph
          (VulkanDataSources reqSources) = vulkanResourcesScene (cFormat,dFormat) targetTree

      --sendM $ print reqSources
      -- wait until loader loads our stuff
      --resources <- sendM $ waitForResourceProcessing resourceLoader (S.empty, S.fromList [basicVertSource, basicImageSource, basicRenderpassSource, basicDescriptorSource, basicPipelineSource])
      resources <- sendM $ waitForResourceProcessing resourceLoader (S.empty, S.insert basicDescriptorSource reqSources)
      --sendM $ print "Resources loaded!"
      let dData = fromJust $ pullResource resources basicDescriptorSource #descriptorSetHelperSource
          --(Just vertices)       = pullResource resources basicVertSource #vkGeometry
          --(Just imagez)         = pullResource resources basicImageSource #textureImage
          rPass = fromJust $ pullResource resources basicRenderpassSource #renderPass
          --(Just pBundle)        = (pullResource resources basicPipelineSource #simplePipeline :: Maybe PipelineBundle)
          --(PipelineBundle pLayout pipe) = pBundle-}

      useDescriptorSetHelperSource dData (slotIndex cSlot) $ resetDescriptorSetHelper

      VK.beginCommandBuffer cBuf (VK.CommandBufferBeginInfo () VZ.zero Nothing)

      let viewport = VK.Viewport 0.0 0.0 (fromIntegral w) (fromIntegral h) 0.0 1.0
      let scissor = VK.Rect2D (Offset2D 0 0) ext2d
      VK.cmdSetViewport cBuf 0 (V.singleton viewport)
      VK.cmdSetScissor cBuf 0 (V.singleton scissor)
      
      let renderarea = Rect2D (Offset2D 0 0) ext2d
      let clearTo = V.fromList [Color (Float32 0 0.5 0.5 1), DepthStencil (ClearDepthStencilValue 1.0 0)]
      --VK.cmdBeginRenderPass cBuf (VK.RenderPassBeginInfo () rPass frameBuf renderarea clearTo) VK.SUBPASS_CONTENTS_INLINE

      let commandTree = vulkanToCommand (VulkanResources resources) (cFormat,dFormat) targetTree
          compiledTree = compileToInvocationTrie commandTree
          flipPerspective = Linear.scaled (V4 1 (-1) 1 1)
          drawParams =    #curTime .== (curTime * 0.016)
                       .+ #windowX .== fromIntegral w
                       .+ #windowY .== fromIntegral h
                       .+ #correctNDC .== flipPerspective
      --sendM $ print compiledTree
      runInvocationTrie compiledTree drawParams (slotIndex cSlot) cBuf frameBuf ext2d
      --vulkanGo commandTree drawParams (slotIndex cSlot) cBuf
      
      --VK.cmdEndRenderPass cBuf
      VK.endCommandBuffer cBuf



