{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Fomorian.Vulkan.PlatformRenderer where

import Control.Monad.Freer
import Control.Exception

import Data.IORef
import Data.Row
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V

import Linear

import qualified Graphics.UI.GLFW as GLFW

import Fomorian.Windowing
import Fomorian.SceneNode
import Fomorian.SceneResources
import Fomorian.NeutralSceneTarget

import qualified Fomorian.Vulkan.WindowBundle as WB
import Fomorian.Vulkan.VulkanMonads
import Fomorian.Vulkan.SwapchainCarousel
import Fomorian.Vulkan.SwapchainBundle
import Fomorian.Vulkan.Resources.VulkanResourcesBase
import Fomorian.Vulkan.Resources.VulkanLoader
import Fomorian.Vulkan.Resources.BoundCommandBuffer
import Fomorian.Vulkan.VulkanTargetTree
import Fomorian.Vulkan.VulkanCommandTree

import Fomorian.PlatformRenderer

import STMLoader.AsyncLoader

import qualified Vulkan.Core10 as VK
import qualified Vulkan.Zero as VZ


data VulkanRendererState =
  VulkanRendererState {
    windowBundle :: WB.WindowBundle,
    vulkanLoader :: AsyncLoader VulkanDataSource VulkanResource VulkanError,
    vulkanSwapchainCarousel :: SwapchainCarousel,
    vulkanStats :: IORef RenderStats
  }

vulkanWrapRender :: (Int,Int) -> (VulkanRendererState -> IO ()) -> IO ()
vulkanWrapRender (w,h) wrapped =
  let initConfig = WB.WindowInitConfig
                    "Vulkan App"  -- application name
                    "Vulkan Test" -- window title
                      Nothing      -- allocator
                      1            -- auxiliary queue count
                      False        -- enable debug validation layers?
  in do
    WB.withWindowBundle initConfig $ \wb -> do
      let pd = WB.physicalDeviceHandle (WB.vulkanDeviceBundle wb)
      deviceProps <- VK.getPhysicalDeviceProperties pd
      let deviceLimits = VK.limits deviceProps
      putStrLn $ "buffer-image granularity = " ++ show (VK.bufferImageGranularity deviceLimits)
      -- fork off a queue submit thread using the aux queue
      let aQueue = head $ WB.auxiliaryQueues $ WB.vulkanDeviceBundle wb
      let prebuiltResources = M.empty
      boundQueue <- aQueue `seq` forkBoundSubmitter wb aQueue
      loaderInfo <- startLoader wb boundQueue prebuiltResources
      let win = WB.windowHandle wb
      let stats = RenderStats win (V2 w h) 0
      statsRef <- newIORef stats

      let runV = runM . runVulkanMonad wb

      finally 
        (runV $ withCarousel 2 $ \swc -> sendM $ wrapped (VulkanRendererState wb loaderInfo swc statsRef))
        (do
          endLoader loaderInfo
          endBoundSubmitter boundQueue
          )

vulkanRenderFrame :: VulkanRendererState -> SceneGraph NeutralSceneTarget DefaultDrawFrameParams -> Rec DefaultDrawFrameParams -> IO Bool
vulkanRenderFrame (VulkanRendererState wb ld swc stats) scene frameData = do
  modifyIORef stats (\s -> s { frameCount = (frameCount s) + 1 })
  f <- fmap frameCount $ readIORef stats
  runM . runVulkanMonad wb $ presentNextSlot swc (runOneFrame swc ld scene f)
  return False
    where
      runOneFrame swc resourceLoader sceneGraph curTime cBuf frameBuf cSlot = do
        sb <- sendM $ readIORef (bundleRef swc)
        let (SwapchainPresentInfo cFormat dFormat ext2d@(VK.Extent2D w h)) = swapchainPresentInfo sb

        let basicRenderpassFormat = (cFormat,dFormat)
            targetTree = neutralToVulkanTarget sceneGraph
            (VulkanDataSources reqSources) = vulkanResourcesScene (cFormat,dFormat) targetTree

        -- right now loading is forced to be synchronous        
        resources <- sendM $ waitForResourceProcessing resourceLoader (S.empty, reqSources)

        VK.beginCommandBuffer cBuf (VK.CommandBufferBeginInfo () VZ.zero Nothing)

        let viewport = VK.Viewport 0.0 0.0 (fromIntegral w) (fromIntegral h) 0.0 1.0
        let scissor = VK.Rect2D (VK.Offset2D 0 0) ext2d
        VK.cmdSetViewport cBuf 0 (V.singleton viewport)
        VK.cmdSetScissor cBuf 0 (V.singleton scissor)
        
        let commandTree = vulkanToCommand (VulkanResources resources) (cFormat,dFormat) targetTree
            compiledTree = compileToInvocationTrie commandTree
            flipPerspective = Linear.scaled (V4 1 (-1) 1 1)
            drawParams =   #curTime .== (fromIntegral curTime * 0.016)
                        .+ #windowX .== fromIntegral w
                        .+ #windowY .== fromIntegral h
                        .+ #correctNDC .== flipPerspective

        runInvocationTrie compiledTree drawParams (slotIndex cSlot) cBuf frameBuf ext2d
        
        VK.endCommandBuffer cBuf

vulkanRendererFunctions :: PlatformRendererFunctions VulkanRendererState
vulkanRendererFunctions =
  PlatformRendererFunctions {
    wrapRenderLoop = vulkanWrapRender,
    getRendererStats = readIORef . vulkanStats,
    runRenderFrame = vulkanRenderFrame
  }