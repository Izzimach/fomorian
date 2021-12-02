{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

-- | This module handles the ring-buffer that buffers drawing frames to the swapchain.
--   We need several copies of certain objects (command buffers, semaphores, etc.) since the resources
--   used in drawing a particular frame may still be in use while we are drawing the next frame. We could wait for the
--   previous frame to "complete" and re-use those resources but instead we keep several copies and cycle through them
--   round-robin. If we draw faster then the presentation layer all the slots will get used up and we'll have to wait
--   on a fence for a slot to open up.
--
--   The number of slots in the carousel relates to how many frames can be in-flight while drawing the next frame. For
--   double-buffering we would have two slots while triple-buffering would have three slots. Instead here we just
--   use N slots.
module Fomorian.Vulkan.SwapchainCarousel where

import Control.Exception
import Control.Monad (when)
import Control.Monad.Freer
import Control.Monad.Freer.Error

import Foreign.Storable (sizeOf)
import Foreign.Ptr (nullPtr)

import Data.Word (Word32)
import Data.IORef (IORef(..), newIORef, readIORef, writeIORef, modifyIORef)
import Data.Vector (Vector(..), fromList, singleton, (!))
import qualified Data.Vector as V

import Vulkan.Core10 (Semaphore, Fence, CommandPool, CommandBuffer, Queue, Framebuffer, DescriptorSet, DescriptorPool)
import qualified Vulkan.Core10 as VK
import qualified Vulkan.Zero as VZ
import Vulkan.Exception
import Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_KHR_swapchain as VKSWAPCHAIN

import Fomorian.Vulkan.WindowBundle
import Fomorian.Vulkan.VulkanMonads
import Fomorian.Vulkan.Resources.DataBuffers
import Fomorian.Vulkan.Resources.DescriptorSets
import Fomorian.Vulkan.Resources.VulkanResourcesBase
import Fomorian.Vulkan.SwapchainBundle

import qualified Graphics.UI.GLFW as GLFW


data CarouselSlot =
  CarouselSlot
  {
    slotIndex :: Int,
    startSlotSemaphore :: Semaphore,
    endSlotSemaphore :: Semaphore,
    commandCompleted :: Fence,
    slotCommandBuffer :: CommandBuffer
  }
  deriving (Eq, Show)

data SwapchainCarousel =
  SwapchainCarousel
  {
    nextSlot :: IORef Int,
    cPool    :: CommandPool,
    gfxQ     :: Queue,
    presentQ :: Queue,
    runSlots :: Vector CarouselSlot,
    bundleRef :: IORef SwapchainBundle
  }

withCarousel :: (InVulkanMonad effs) => Int -> (SwapchainCarousel -> Eff '[VulkanMonad,IO] x) -> Eff effs x
withCarousel slotCount wrapped = do
  -- we need to push the freer monads through 'bracket'
  wb <- getWindowBundle
  let runV = runM . runVulkanMonad wb
  let goCarousel = runV $ makeCarousel 2 Nothing
  let stopCarousel swc = runV $ do flushCarousel swc Nothing; destroyCarousel swc
  sendM $ bracket goCarousel stopCarousel (runV . wrapped)

-- | Build relevant objects for the given swapchain
makeCarousel :: (InVulkanMonad effs) => Int -> Maybe SwapchainBundle -> Eff effs SwapchainCarousel
makeCarousel slotCount previousSwapchainBundle = do
  d <- getDevice
  deviceBundle <- vulkanDeviceBundle <$> getWindowBundle
  let gQ = graphicsQueue deviceBundle
  let gQIndex = graphicsQueueFamilyIndex deviceBundle
  let pQ = presentQueue deviceBundle
  cPool <- VK.createCommandPool d (VK.CommandPoolCreateInfo VK.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT gQIndex) Nothing 
  swapchain <- makeSwapchainBundle previousSwapchainBundle
  let allocInfo = VK.CommandBufferAllocateInfo cPool VK.COMMAND_BUFFER_LEVEL_PRIMARY (fromIntegral slotCount)
  cmdBuffers <- VK.allocateCommandBuffers d allocInfo
  slots <- mapM makeSlot (V.zip cmdBuffers (V.fromList [0..(slotCount-1)]))

  curSlotIndex <- sendM $ newIORef 0
  swapchainRef <- sendM $ newIORef swapchain
  return $ SwapchainCarousel curSlotIndex cPool gQ pQ slots swapchainRef
    where
      makeSlot (cBuf,ix) = do
        d <- getDevice
        let alloc = Nothing
        startSem <- VK.createSemaphore d (VK.SemaphoreCreateInfo () VZ.zero) alloc
        endSem <- VK.createSemaphore d (VK.SemaphoreCreateInfo () VZ.zero) alloc
        completedFence <- VK.createFence d (VK.FenceCreateInfo () VK.FENCE_CREATE_SIGNALED_BIT) alloc
        return $ CarouselSlot ix startSem endSem completedFence cBuf

destroyCarousel :: (InVulkanMonad effs) => SwapchainCarousel -> Eff effs ()
destroyCarousel (SwapchainCarousel slotRef cPool gQ pQ slots sbRef) = do
  d <- getDevice
  mapM_ destroySlot slots
  let cBufs = fmap slotCommandBuffer slots
  VK.freeCommandBuffers d cPool cBufs
  -- don't need to destroy descriptor sets, they get destroyed when the pool is destroyed
  sb <- sendM $ readIORef sbRef
  destroySwapchainBundle sb
  VK.destroyCommandPool d cPool Nothing
    where
      destroySlot (CarouselSlot _ startSem endSem completedFence cBuf) = do
        d <- getDevice
        let alloc = Nothing
        VK.destroySemaphore d startSem alloc
        VK.destroySemaphore d endSem alloc
        VK.destroyFence d completedFence alloc

-- | Runs the next slot (waiting if it's still in-process) and present it via
--  the swapchain
presentNextSlot :: (InVulkanMonad effs) => SwapchainCarousel -> (CommandBuffer -> Framebuffer -> CarouselSlot -> Eff effs ()) -> Eff effs ()
presentNextSlot swc f = do
  d <- getDevice
  swapBundle <- sendM $ readIORef (bundleRef swc)
  let swapHandle = swapchainHandle swapBundle
  let slotRef = nextSlot swc
  slotIx <- sendM $ readIORef slotRef 
  let cSlot@(CarouselSlot _ix startSem endSem cmdFence cmdBuf) = runSlots swc ! slotIx
  -- both acquire and present can throw exceptions if the swapchain is invalid, so we catch those exceptions and return error
  -- results. Error results will throw errors via freer-simple's 'Error' monad and get processed at the bottom
  goFrame <- runError @VK.Result $ do
    -- catch invalid swapchain exceptions and rebuild the swapchain
    (acquireResult, imgIndex) <- sendM $ handle catchAcquireException (VKSWAPCHAIN.acquireNextImageKHR d swapHandle maxBound startSem VK.NULL_HANDLE)
    when (acquireResult /= VK.SUCCESS) $ throwError acquireResult
    _ <- VK.waitForFences d (fromList [cmdFence]) True maxBound
    -- Note that in this section the current fence is not and will not be signalled, so you can't wait for it!
    -- This is important if handling exceptions. Luckily exceptions due to swapchain errors don't happen in this section so it's not
    -- normally an issue.
    let (SwapchainPerImageData _ _ _ fb) = swapchainImages swapBundle ! fromIntegral imgIndex
    VK.resetFences d (fromList [cmdFence])
    VK.resetCommandBuffer cmdBuf VZ.zero
    raise $ f cmdBuf fb cSlot
    let submitInfo = SomeStruct $ VK.SubmitInfo () (singleton startSem) (fromList [VK.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]) (fromList [VK.commandBufferHandle cmdBuf]) (fromList [endSem])
    VK.queueSubmit (gfxQ swc) (fromList [submitInfo]) cmdFence
    -- here the fence will again be signalled upon command completion so you can wait for it
    let presentInfo = PresentInfoKHR () (fromList [endSem]) (fromList [swapHandle]) (fromList [imgIndex]) nullPtr
    presentResult <- sendM $ handle catchSwapchainException (VKSWAPCHAIN.queuePresentKHR (presentQ swc) presentInfo)
    when (presentResult /= VK.SUCCESS) $ throwError presentResult
    sendM $ modifyIORef slotRef (\x -> (x+1) `mod` length (runSlots swc))
    win <- windowHandle <$> getWindowBundle
    sendM $ do
      GLFW.pollEvents
      GLFW.windowShouldClose win
  case goFrame of
    Right shouldTerminate -> return ()
    Left result -> if result == VK.ERROR_OUT_OF_DATE_KHR || result == VK.SUBOPTIMAL_KHR
                   then rebuildSwapchain
                   else error ("Swapchain exception: " ++ show result)
  where
    -- | exception handler that catches 
    catchSwapchainException :: VulkanException -> IO VK.Result
    catchSwapchainException (VulkanException vkResult) =
      if vkResult == VK.ERROR_OUT_OF_DATE_KHR || vkResult == VK.SUBOPTIMAL_KHR
      then pure vkResult
      else error $ "Vulkan exception in SwapchainCarousel: " ++ show vkResult

    catchAcquireException :: VulkanException -> IO (VK.Result,Word32)
    catchAcquireException e = fmap (\x -> (x,0)) (catchSwapchainException e)

    rebuildSwapchain :: (InVulkanMonad effs) => Eff effs ()
    rebuildSwapchain = do
      -- need to wait for previous frames to complete
      flushCarousel swc Nothing
      let swRef = bundleRef swc
      sb <- sendM $ readIORef swRef
      -- make a new swapchain, possibly re-using parts of the old one
      newSB <- makeSwapchainBundle (Just sb)
      destroySwapchainBundle sb
      sendM $ writeIORef swRef newSB


-- | Wait for any frames currently being drawn to finish (or at least the command buffers)
--   The @Maybe CarouselSlot@ passed in is a slot which has a zombie fence and so we shouldn't wait for that slot
flushCarousel :: (InVulkanMonad effs) => SwapchainCarousel -> Maybe CarouselSlot -> Eff effs ()
flushCarousel swc ignoreThis = do
  d <- getDevice
  let fenceFilter = case ignoreThis of
                      Nothing -> const True
                      Just t  -> (/= t)
      extractFence = (\(CarouselSlot _ _ _ f _) -> f)
      fences = fmap extractFence (V.filter fenceFilter $ runSlots swc)
  _ <- VK.waitForFences d fences True maxBound
  return ()
