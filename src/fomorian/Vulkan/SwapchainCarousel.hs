
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

import Data.Vector(Vector(..))
import Data.IORef (IORef(..))


import Vulkan.Core10 (Semaphore, Fence, CommandPool, CommandBuffer, Queue)
import qualified Vulkan.Core10 as VK
import Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_KHR_swapchain as VKSWAPCHAIN

data CarouselSlot =
  CarouselSlot
  {
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
    runSlots :: Vector CarouselSlot
  }

-- | Runs the next slot (waiting if it's still in-process) and present it via
--  the swapchain
presentNextSlot :: SwapchainCarousel -> (CommandBuffer -> IO ()) -> IO ()
presentNextSlot swc f = do
  return ()

-- | Build relevant objects for the given swapchain
makeCarousel :: CommandPool -> Queue -> Queue -> IO SwapchainCarousel
makeCarousel = undefined

destroyCarousel :: SwapchainCarousel -> IO ()
destroyCarousel = undefined