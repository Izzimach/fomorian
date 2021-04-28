{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Fomorian.Vulkan.Vulkan where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Bits
import Data.ByteString (ByteString, readFile)
import Data.Foldable
import Data.IORef
import Data.Vector ((!), (//), Vector, empty, findIndex, fromList)
import Data.Word (Word16, Word32, Word64)
import Fomorian.Windowing
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import GHC.Int
import qualified Graphics.UI.GLFW as GLFW
import Linear (V2 (..), V3 (..))
import System.FilePath
import Vulkan.CStruct.Extends
import Vulkan.Core10 as VKCORE
import Vulkan.Core10.DeviceInitialization as VKDI
import Vulkan.Core10.MemoryManagement as VKMEM
import Vulkan.Exception
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_EXT_validation_features
import Vulkan.Extensions.VK_KHR_surface as VKSURFACE
import Vulkan.Extensions.VK_KHR_swapchain as VKSWAPCHAIN
import Vulkan.Extensions.VK_KHR_win32_surface
import Vulkan.Zero

import Fomorian.Vulkan.WindowEtc
import Fomorian.Vulkan.SwapChainEtc


cMAX_FRAMES_IN_FLIGHT :: Int
cMAX_FRAMES_IN_FLIGHT = 2

main :: IO ()
main = do
  --let config = instanceConfig
  let config = validatedInstance defaultInstanceConfig
  let allocator = Nothing
  let windowConfig = WindowInitData 600 400 "Vulkan test window" NoOpenGL
  withInstance config allocator bracket $ \inst -> do
    withWindowEtc inst windowConfig cMAX_FRAMES_IN_FLIGHT bracket $ \windowETC -> do
      renderLoop windowETC allocator
      deviceWaitIdle (vkDevice windowETC)

-- | Main render loop. Initialize the set of IORefs to track which frameBuffers
--   are 'in flight' and then updates this every frame.
renderLoop :: WindowEtc -> Maybe AllocationCallbacks -> IO ()
renderLoop windowEtc allocator = do
  inFlightTrackerInit >>= go 0
  return ()
  where
    inFlightTrackerInit :: IO (Vector Fence)
    inFlightTrackerInit = do
      swEtc <- readIORef (swapChainRef windowEtc)
      let imageCount = length (swapchainFramebuffers swEtc)
      return $ fromList $ fmap (\_ -> NULL_HANDLE) [1 .. imageCount]
    go currentFrame inFlightTracker = do
      GLFW.pollEvents
      shouldClose <- GLFW.windowShouldClose (windowHandle windowEtc)
      if shouldClose
        then return ()
        else do
          inFlightTracker' <- renderFrame windowEtc currentFrame inFlightTracker allocator
          go ((currentFrame + 1) `mod` cMAX_FRAMES_IN_FLIGHT) inFlightTracker'

-- | Render a single frame. Gets most of the relevant info from the 'WindowEtc'
--   record. Also takes in and updates the vector of fences for the 'in flight' frame buffers.
renderFrame :: WindowEtc -> Int -> Vector Fence -> Maybe AllocationCallbacks -> IO (Vector Fence)
renderFrame windowEtc currentFrame inFlight allocator = do
  swapchainEtc <- readIORef (swapChainRef windowEtc)
  let device = vkDevice windowEtc
  let swap = theSwapchain swapchainEtc
  let iaSemaphore = imageAvailableSemaphores windowEtc ! currentFrame
  let sgSemaphore = renderingFinishedSemaphores windowEtc ! currentFrame
  let thisFence = fences windowEtc ! currentFrame
  waitForFences device (fromList [thisFence]) True maxBound
  -- if the swapchain is invalid (perhaps due to window resizing) then acquireNextImageKHR
  -- or queuePresentKHR will throw an ERROR_OUT_OF_DATE_KHR exception, so we need to catch
  -- that and then recreate the swapchain
  runResult <- try $
    do
      (_, imgIndex) <- acquireNextImageKHR device swap maxBound iaSemaphore NULL_HANDLE
      -- Lookup this image in the 'inFlight' vector; if there is a fence there then the
      -- frame was in flight and we need to wait on that fence for the relevant queue to complete.
      let imageFence = inFlight ! fromIntegral imgIndex
      if (imageFence /= NULL_HANDLE)
        then waitForFences device (fromList [imageFence]) True maxBound
        else return SUCCESS
      let swcBuffer = (swapchainCommandBuffers swapchainEtc) ! (fromIntegral imgIndex)
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
      queuePresentKHR (presentQueue windowEtc) presentInfo
      return $ inFlight // [(fromIntegral imgIndex, thisFence)]
  -- If the swapchain was invalid (ERROR_OUT_OF_DATE_KHR) we'll have an exception here,
  -- in which case we need to recreate the swapchain.
  case runResult of
    Right result -> return result
    Left (VulkanException ERROR_OUT_OF_DATE_KHR) -> do
      let chosen = vkChosen windowEtc
      let cpool = (cmdPool windowEtc)
      deviceWaitIdle device -- wait until previous command/frames are done
      let tRes = transients windowEtc
      newswapchain <- recreateSwapChainEtc device cpool tRes swapchainEtc chosen (surfaceRef windowEtc) allocator
      liftIO $ writeIORef (swapChainRef windowEtc) newswapchain
      return inFlight
    Left exc -> throw exc


--
-- extra functions to dump vulkan debug text.
--

debugInstance :: Instance -> IO ()
debugInstance i = do
  (_, layerz) <- liftIO enumerateInstanceLayerProperties
  (_, extensionz) <- enumerateInstanceExtensionProperties Nothing
  putStrLn (show extensionz)
  --putStrLn (show layerz)
  (_, devices) <- enumeratePhysicalDevices i
  traverse_ deviceInfo devices

deviceInfo :: (MonadIO m) => PhysicalDevice -> m ()
deviceInfo p = do
  (_, extensions) <- enumerateDeviceExtensionProperties p Nothing
  (_, layers) <- enumerateDeviceLayerProperties p
  liftIO $ traverse_ myPrint extensions
  liftIO $ traverse_ myPrint layers
  --myPrint =<< getPhysicalDeviceFeatures p
  myPrint =<< getPhysicalDeviceQueueFamilyProperties p
  myPrint =<< getPhysicalDeviceProperties p
  where
    --myPrint =<< getPhysicalDeviceMemoryProperties p

    myPrint :: (MonadIO m, Show a) => a -> m ()
    myPrint = liftIO . putStrLn . show
