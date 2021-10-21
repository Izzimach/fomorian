{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Module for vulkan code. When in the VulkanMonad code can access a lot of the standard data needed to call
--   vulkan functions such as the device and physical device. Also provides a route to allocate/deallocate device memory.
module Fomorian.Vulkan.VulkanMonads where

import Control.Concurrent (forkOS)
import Control.Concurrent.STM ( TMVar, atomically, putTMVar, takeTMVar )
import Control.Exception (bracket)
import Control.Monad.Freer as CMF
import Control.Monad.Freer.Reader (Reader(..), ask, runReader)

import Data.Vector ((!), singleton, empty, fromList)



import Fomorian.SimpleMemoryArena

import Fomorian.Vulkan.WindowBundle
import Fomorian.Vulkan.Resources.DeviceMemoryTypes (AbstractMemoryType(..))
import Fomorian.Vulkan.Resources.DeviceMemoryAllocator


import Vulkan.Core10 (Device, PhysicalDevice, MemoryRequirements(..), DeviceSize, commandBufferHandle)
import qualified Vulkan.Core10 as VK
import qualified Vulkan.Zero as VZ
import Vulkan.CStruct.Extends ( SomeStruct(SomeStruct) )



-- | The 'Vulkan Monad'. Provides access to a memory allocator and the basic top-level handles like instance, device, etc.
data VulkanMonad r where
  AllocateV :: MemoryRequirements -> AbstractMemoryType -> VulkanMonad (MemoryAllocation DeviceSize)
  DeallocateV :: MemoryAllocation DeviceSize -> VulkanMonad ()
  GetWindowBundle :: VulkanMonad WindowBundle

type InVulkanMonad effs = (Member VulkanMonad effs, LastMember IO effs)



-- | When in the loader monad you can use this to allocate memory for buffers
allocateV :: Member VulkanMonad effs => MemoryRequirements -> AbstractMemoryType -> Eff effs (MemoryAllocation DeviceSize)
allocateV req memType = send (AllocateV req memType)

deallocateV :: Member VulkanMonad effs => MemoryAllocation DeviceSize -> Eff effs ()
deallocateV allocation = send (DeallocateV allocation)

-- | When in the loader monad you can run this to get the current vulkan device
getDevice :: Member VulkanMonad effs => Eff effs Device
getDevice = Fomorian.Vulkan.WindowBundle.deviceHandle . vulkanDeviceBundle <$> getWindowBundle

getPhysicalDevice :: Member VulkanMonad effs => Eff effs PhysicalDevice
getPhysicalDevice = Fomorian.Vulkan.WindowBundle.physicalDeviceHandle . vulkanDeviceBundle <$> getWindowBundle

getWindowBundle :: Member VulkanMonad effs => Eff effs WindowBundle
getWindowBundle = send GetWindowBundle

-- | Interpreter for the VulkanMonad monad
runVulkanMonad :: LastMember IO effs => WindowBundle -> Eff (VulkanMonad ': effs) ~> Eff effs
runVulkanMonad w = runReader w . vulkanMemToState
  where
    vulkanMemToState :: LastMember IO effz => Eff (VulkanMonad ': effz) ~> Eff (Reader WindowBundle ': effz)
    vulkanMemToState = reinterpret $ \case
        AllocateV m t -> do
          wb <- ask
          result <- sendM $ allocateSTM (memoryManager wb) m t
          case result of
            Nothing -> error "argh"
            Just x  -> return x
        DeallocateV b -> do
          wb <- ask
          sendM $ deallocateSTM (memoryManager wb) b
          return ()
        GetWindowBundle -> ask



allocateSTM :: TMVar (MemoryAllocatorState AbstractMemoryType) -> MemoryRequirements -> AbstractMemoryType -> IO (Maybe (MemoryAllocation DeviceSize))
allocateSTM memVar (MemoryRequirements size alignment allowedTypeBits) memType = do
  -- we use take so that no one else can allocate until this function is done
  memState <- atomically (takeTMVar memVar)
  (alloc, memState') <- allocateDeviceMemory memState size (MemAlign alignment) memType allowedTypeBits
  atomically $ putTMVar memVar memState'
  return alloc
  
deallocateSTM :: TMVar (MemoryAllocatorState AbstractMemoryType) -> MemoryAllocation DeviceSize -> IO ()
deallocateSTM memVar memBlock = do
  memState <- atomically (takeTMVar memVar)
  freeResult <- freeDeviceMemory memState memBlock
  case freeResult of
    Nothing -> atomically $ putTMVar memVar memState
    Just memState' -> atomically $ putTMVar memVar memState'


-- | This vulkan monad allows you to submit commands to a queue. The particulars of the commmand and queue (synchronization etc) are
--   opaque; to enforce specific situations you should make a new effect.
data OneShotSubmitter r where
  OneShotCommand :: (VK.CommandBuffer -> IO a) -> OneShotSubmitter a

oneShotCommand :: (Member OneShotSubmitter effs) => (VK.CommandBuffer -> IO a) -> Eff effs a
oneShotCommand cmd = send (OneShotCommand cmd)



-- A default implementation of OneShotSubmitter that just runs the command and waits until the command buffer finishes. Probably not too useful except as an example
runOneShotSimple :: (LastMember IO effs, Member VulkanMonad effs) => VK.CommandPool -> VK.Queue -> VK.Fence -> Eff (OneShotSubmitter ': effs) ~> Eff effs
runOneShotSimple cPool vkQ fence = interpret $ \(OneShotCommand cmd) -> do
  d <- getDevice
  let allocInfo = VK.CommandBufferAllocateInfo cPool VK.COMMAND_BUFFER_LEVEL_PRIMARY 1
  cmdBuffers <- VK.allocateCommandBuffers d allocInfo
  let cBuf = cmdBuffers ! 0
  let beginInfo = VK.CommandBufferBeginInfo () VK.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT Nothing
  VK.beginCommandBuffer cBuf beginInfo
  result <- sendM $ cmd cBuf
  VK.endCommandBuffer cBuf
  let submitInfo = SomeStruct $ VK.SubmitInfo () empty empty (fromList [commandBufferHandle cBuf]) empty
  VK.resetFences d (fromList [fence])
  VK.queueSubmit vkQ (fromList [submitInfo]) fence
  _ <- VK.waitForFences d (fromList [fence]) True maxBound
  VK.freeCommandBuffers d cPool (singleton cBuf)
  return result


