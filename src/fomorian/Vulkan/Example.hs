{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Fomorian.Vulkan.Example where

import Control.Exception
import Control.Monad.IO.Class
import Data.Foldable
import Data.IORef
import Data.Vector ((!), (//), Vector, fromList)
import Fomorian.Windowing
import Foreign.Ptr
import qualified Graphics.UI.GLFW as GLFW
import Vulkan.CStruct.Extends
import Vulkan.Core10 as VKCORE
import Vulkan.Exception
import Vulkan.Extensions.VK_KHR_swapchain as VKSWAPCHAIN

import Fomorian.Vulkan.WindowEtc
import Fomorian.Vulkan.SwapChainEtc
import Fomorian.Vulkan.TransientResources


cMAX_FRAMES_IN_FLIGHT :: Int
cMAX_FRAMES_IN_FLIGHT = 2

data InFlightTracker = InFlightTracker Int (Vector Fence)

mkInFlightTracker :: WindowEtc -> IO (InFlightTracker)
mkInFlightTracker windowEtc = do
  swEtc <- readIORef (swapChainRef windowEtc)
  let imageCount = length (swapchainFramebuffers swEtc)
  let flightFences = fromList $ fmap (\_ -> NULL_HANDLE) [1 .. imageCount]
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
        if (shouldClose || (p == GLFW.KeyState'Pressed))
          then return ()
          else do
            renderFrame windowEtc inFlightTrackerRef allocator
            go inFlightTrackerRef

-- | Render a single frame. Gets most of the relevant info from the 'WindowEtc'
--   record. Also takes in and updates the vector of fences for the 'in flight' frame buffers.
renderFrame :: WindowEtc -> IORef (InFlightTracker) {-Int -> Vector Fence-} -> Maybe AllocationCallbacks -> IO ()
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
      _ <- if (imageFence /= NULL_HANDLE)
           then waitForFences device (fromList [imageFence]) True maxBound
           else return SUCCESS
      let swcBuffer = (swapchainCommandBuffers swapchainEtc) ! (fromIntegral imgIndex)
      let uniformBuffer = (swapchainPerFrameResources swapchainEtc) ! (fromIntegral imgIndex)
      updateUniformBuffer device (uniforms uniformBuffer) ((fromIntegral currentFrame) * 0.016) (VKSWAPCHAIN.imageExtent $ swapchainCreateInfo swapchainEtc)
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
  putStrLn (show extensionz)
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
    myPrint = liftIO . putStrLn . show
