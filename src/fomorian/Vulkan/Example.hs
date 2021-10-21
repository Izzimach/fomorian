{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Fomorian.Vulkan.Example where

import Control.Exception
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Control.Monad.Freer

import Data.Foldable
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Vector as V ((!), (//), Vector, fromList, empty, singleton)
import Data.Row

import Linear

import Foreign.Ptr

import Text.Pretty.Simple (pPrint)

import LoadUnload
import AsyncLoader

import qualified Graphics.UI.GLFW as GLFW
import Vulkan.CStruct.Extends
import Vulkan.Core10 as VK
import Vulkan.Zero as VZ
import Vulkan.Exception
import Vulkan.Extensions.VK_KHR_swapchain as VKSWAPCHAIN

import Fomorian.Windowing
import Fomorian.SceneResources
import Fomorian.SimpleMemoryArena (MemoryAlignment(..))
import qualified Fomorian.Vulkan.WindowBundle as WB

import Fomorian.Vulkan.VulkanMonads
import Fomorian.Vulkan.SwapchainCarousel
import Fomorian.Vulkan.SwapchainBundle
import Fomorian.Vulkan.Resources.DescriptorSets (UniformBufferObject(..), zeroOnePerspective, updateUniformBuffer)
import Fomorian.Vulkan.Resources.VulkanResourcesBase
import Fomorian.Vulkan.Resources.DeviceMemoryTypes
import Fomorian.Vulkan.Resources.BoundCommandBuffer
import Fomorian.Vulkan.Resources.VulkanLoader

-- legacy modules
--import Fomorian.Vulkan.WindowEtc
--import Fomorian.Vulkan.SwapChainEtc
--import Fomorian.Vulkan.TransientResources

{-
cMAX_FRAMES_IN_FLIGHT :: Int
cMAX_FRAMES_IN_FLIGHT = 2

data InFlightTracker = InFlightTracker Int (Vector Fence)

mkInFlightTracker :: WindowEtc -> IO InFlightTracker
mkInFlightTracker windowEtc = do
  swEtc <- readIORef (swapChainRef windowEtc)
  let imageCount = length (swapchainFramebuffers swEtc)
  let flightFences = fromList $ fmap (const NULL_HANDLE) [1 .. imageCount]
  return (InFlightTracker 0 flightFences)


main :: IO ()
main = do
  --let config = instanceConfig
  let allocator = Nothing
  let vulkanConfig = VulkanConfig (addValidation defaultInstanceConfig) cMAX_FRAMES_IN_FLIGHT
  let windowConfig = WindowInitData 600 400 "Vulkan test window" NoOpenGL
  withWindowEtc vulkanConfig windowConfig allocator bracket $ \windowETC -> do
    renderLoop windowETC allocator
    deviceWaitIdle (vkDevice windowETC)

-- | Main render loop. Initialize the set of IORefs to track which frameBuffers
--   are 'in flight' and then updates this every frame.
renderLoop :: WindowEtc -> Maybe AllocationCallbacks -> IO ()
renderLoop windowEtc allocator = do
  inFlightData <- mkInFlightTracker windowEtc 
  inFlightRef <- newIORef inFlightData
  go inFlightRef
  return ()
    where
      go inFlightTrackerRef = do
        GLFW.pollEvents
        p <- GLFW.getKey (windowHandle windowEtc) GLFW.Key'Escape
        shouldClose <- GLFW.windowShouldClose (windowHandle windowEtc)
        if shouldClose || (p == GLFW.KeyState'Pressed)
          then return ()
          else do
            renderFrame windowEtc inFlightTrackerRef allocator
            go inFlightTrackerRef

-- | Render a single frame. Gets most of the relevant info from the 'WindowEtc'
--   record. Also takes in and updates the vector of fences for the 'in flight' frame buffers.
renderFrame :: WindowEtc -> IORef InFlightTracker {-Int -> Vector Fence-} -> Maybe AllocationCallbacks -> IO ()
renderFrame windowEtc inFlightTrackerRef allocator = do
  swapchainEtc <- readIORef (swapChainRef windowEtc)
  let device = vkDevice windowEtc
  let swap = theSwapchain swapchainEtc
  (InFlightTracker currentFrame inFlight) <- readIORef inFlightTrackerRef
  let frameLookup = currentFrame `mod` cMAX_FRAMES_IN_FLIGHT
  let iaSemaphore = imageAvailableSemaphores windowEtc ! frameLookup
  let sgSemaphore = renderingFinishedSemaphores windowEtc ! frameLookup
  let thisFence = fences windowEtc ! frameLookup
  _ <- waitForFences device (fromList [thisFence]) True maxBound
  -- if the swapchain is invalid (perhaps due to window resizing) then acquireNextImageKHR
  -- or queuePresentKHR will throw an ERROR_OUT_OF_DATE_KHR exception, so we need to catch
  -- that and then recreate the swapchain
  runResult <- try $
    do
      (_, imgIndex) <- acquireNextImageKHR device swap maxBound iaSemaphore NULL_HANDLE
      -- Lookup this image in the 'inFlight' vector; if there is a fence there then the
      -- frame was in flight and we need to wait on that fence for the relevant queue to complete.
      let imageFence = inFlight ! fromIntegral imgIndex
      _ <- if imageFence /= NULL_HANDLE
           then waitForFences device (fromList [imageFence]) True maxBound
           else return SUCCESS
      let swcBuffer = swapchainCommandBuffers swapchainEtc ! fromIntegral imgIndex
      let uniformBuffer = swapchainPerFrameResources swapchainEtc ! fromIntegral imgIndex
      updateUniformBuffer device (uniforms uniformBuffer) (fromIntegral currentFrame * 0.016) (VKSWAPCHAIN.imageExtent $ swapchainCreateInfo swapchainEtc)
      let submitInfo =
            SomeStruct $
               SubmitInfo
                ()
                (fromList [iaSemaphore])
                (fromList [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT])
                (fromList [commandBufferHandle swcBuffer])
                (fromList [sgSemaphore])
      resetFences device (fromList [thisFence])
      queueSubmit (graphicsQueue windowEtc) (fromList [submitInfo]) thisFence
      let presentInfo =
            PresentInfoKHR
              ()
              (fromList [sgSemaphore])
              (fromList [theSwapchain swapchainEtc])
              (fromList [imgIndex])
              nullPtr
      _ <- queuePresentKHR (presentQueue windowEtc) presentInfo
      return $ inFlight // [(fromIntegral imgIndex, thisFence)]
  -- If the swapchain was invalid (ERROR_OUT_OF_DATE_KHR) we'll have an exception here,
  -- in which case we need to recreate the swapchain.
  case runResult of
    Right result -> do
      writeIORef inFlightTrackerRef (InFlightTracker (currentFrame+1) result)
    Left (VulkanException ERROR_OUT_OF_DATE_KHR) -> do
      deviceWaitIdle device -- wait until previous command/frames are done
      rebuildSwapChain windowEtc allocator
    Left exc -> throw exc


--
-- extra functions to dump vulkan debug text.
--

debugInstance :: Instance -> IO ()
debugInstance i = do
  --(_, layerz) <- liftIO enumerateInstanceLayerProperties
  (_, extensionz) <- enumerateInstanceExtensionProperties Nothing
  print extensionz
  --putStrLn (show layerz)
  (_, devices) <- enumeratePhysicalDevices i
  traverse_ deviceInfo devices

deviceInfo :: (MonadIO m) => PhysicalDevice -> m ()
deviceInfo p = do
  (_, extensions) <- enumerateDeviceExtensionProperties p Nothing
  (_, layerz) <- enumerateDeviceLayerProperties p
  liftIO $ traverse_ myPrint extensions
  liftIO $ traverse_ myPrint layerz
  --myPrint =<< getPhysicalDeviceFeatures p
  myPrint =<< getPhysicalDeviceQueueFamilyProperties p
  myPrint =<< getPhysicalDeviceProperties p
  where
    --myPrint =<< getPhysicalDeviceMemoryProperties p

    myPrint :: (MonadIO m, Show a) => a -> m ()
    myPrint = liftIO . print

-}

runSomeVulkan :: IO ()
runSomeVulkan = do
  let cfg = WB.WindowInitConfig "app name" "title" Nothing 1 True
  WB.withWindowBundle cfg $ \wb -> do
    -- fork off a queue submit thread using the aux queue
    let aQueue = head $ WB.auxiliaryQueues $ WB.vulkanDeviceBundle wb
    boundQueue <- aQueue `seq` forkBoundSubmitter wb aQueue
    loaderInfo <- startLoader wb boundQueue
    let basicVertSource = DataSource $ IsJust #coordinates2d [(0,0),(1,0),(1,1),(0,0),(1,1),(0,1)]
    let sceneResources = LoaderRequest (S.fromList [DataSource $ IsJust #placeholderSource (), basicVertSource])
    atomically $ writeTVar (stmRequest loaderInfo) sceneResources
    -- wait until loader loads our stuff
    resources <- atomically $ do
      (LoaderResult loaded) <- readTVar (stmResult loaderInfo)
      if M.null loaded
      then retry
      else return loaded
    let (Just (Resource basicVertData)) = M.lookup basicVertSource resources
    pPrint basicVertData
    let vertices = case trial basicVertData #vkGeometry of
                    Left _ -> error "Argh"
                    Right g -> g


    let d = WB.deviceHandle $ WB.vulkanDeviceBundle wb
    let gQ = WB.graphicsQueue $ WB.vulkanDeviceBundle wb
    let gQIndex = WB.graphicsQueueFamilyIndex $ WB.vulkanDeviceBundle wb
    let pQ = WB.presentQueue $ WB.vulkanDeviceBundle wb
    VK.withCommandPool d (CommandPoolCreateInfo VK.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT gQIndex) Nothing bracket $ \gPool -> do
      runM . runVulkanMonad wb $ do
        swc <- makeCarousel gPool gQ pQ 2 Nothing
        forM_ [1..50] (\x -> presentNextSlot swc (clearCmd swc vertices x))
        flushCarousel swc
        destroyCarousel swc

    endLoader loaderInfo
    endBoundSubmitter boundQueue
  where
    clearCmd swc vertGeo curTime cBuf frameBuf cSlot = do
      let swapchainBundle = swapchainB swc
      let windowExtent@(Extent2D w h) = VKSWAPCHAIN.imageExtent $ relevantCreateInfo swapchainBundle
      -- set uniform buffer
      let uBuf = slotUniformBuffer cSlot
      updateUni uBuf (curTime * 0.016) windowExtent
      let rPass = swapchainRenderPass $ swapchainB swc
      let renderarea = Rect2D (Offset2D 0 0) windowExtent
      let clearTo = V.fromList [Color (Float32 1 0 0 1), DepthStencil (ClearDepthStencilValue 1.0 0)]
      let viewport = Viewport 0.0 0.0 (fromIntegral w) (fromIntegral h) 0.0 1.0

      beginCommandBuffer cBuf (CommandBufferBeginInfo () VZ.zero Nothing)
      cmdSetViewport cBuf 0 (V.singleton viewport)
      cmdBeginRenderPass cBuf (RenderPassBeginInfo () rPass frameBuf renderarea clearTo) VK.SUBPASS_CONTENTS_INLINE

      cmdBindPipeline cBuf VK.PIPELINE_BIND_POINT_GRAPHICS (swapchainPipeline swapchainBundle)
      let (GeometryResource (VBuffer vBuf _) ixBuf elements _) = vertGeo
      cmdBindVertexBuffers cBuf 0 (V.singleton vBuf) (V.singleton 0)
      cmdBindDescriptorSets cBuf VK.PIPELINE_BIND_POINT_GRAPHICS (swapchainPipeLayout swapchainBundle) 0 (V.singleton $ slotDescriptorSet cSlot) V.empty
      cmdDraw cBuf (fromIntegral elements * 3) 1 0 0

      cmdEndRenderPass cBuf
      endCommandBuffer cBuf



updateUni :: (InVulkanMonad effs) => UBuffer -> Float -> Extent2D -> Eff effs ()
updateUni ub elapsedTime (Extent2D width height) = do
  -- our shaders use premultiply so matrices need to be transposed
  let modelMatrix = transpose $ mkTransformation (axisAngle (V3 0 0 1) elapsedTime) (V3 (sin elapsedTime) 0 0.1)
  let viewMatrix = transpose $ lookAt (V3 2 2 2) (V3 0 0 0) (V3 0 0 1)
  let scaleMatrix sx sy sz = V4 (V4 sx 0 0 0) (V4 0 sy 0 0) (V4 0 0 sz 0) (V4 0 0 0 1)
  let aspect = fromIntegral width / fromIntegral height
  let projMatrix = transpose $ zeroOnePerspective (45 * 3.14159 / 180.0) aspect 0.1 10 !*! scaleMatrix 1 (-1) 1
  let newUniforms = UBO modelMatrix viewMatrix projMatrix
  updateUniformBuffer ub newUniforms


