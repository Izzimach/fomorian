{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Runs a bound haskell thread which builds command buffers and submits them to a specific queue.
--   When multithreading in vulkan, each thread needs it's own command pool and submissions to a queue
--   need to be synchronized, which usually means submitting from only one thread.
--
--   Unfornately the "threads" in the Haskell run time do not map 1-to-1 onto OS-level threads. So we create
--   a "bound thread" using 'forkOS' which guarantees that the code will stick to a single OS-level thread.
--   Other haskell threads can submit code to run on this thread via a 'TChan'.
--
--   This version does no synchronization except to wait for the command buffer via single fence, so it's mainly used
--   to transfer static resources (vertex buffers, images) from host-visible memory to device-local memory.
module Fomorian.Vulkan.Resources.BoundCommandBuffer where

import Control.Monad.Freer
import Control.Concurrent
import Control.Exception
import Control.Concurrent.STM
    ( TMVar,
      TChan,
      atomically,
      retry,
      newTChan,
      tryReadTChan,
      writeTChan,
      newEmptyTMVarIO,
      putTMVar,
      takeTMVar )

import Data.Vector as V hiding (mapM_)

-- vulkan core has a lot of common variables exposed like 'size' 'buffer' etc so we specifically import only what we need
import Vulkan.Core10 (CommandBuffer, CommandPool, Fence, Queue, SubmitInfo(..), CommandPoolCreateInfo(..))
import qualified Vulkan.Core10 as VK
import Vulkan.Zero as VZ ( Zero(zero) )
import Vulkan.CStruct.Extends


import Fomorian.Vulkan.WindowBundle
import Fomorian.Vulkan.VulkanMonads


newtype CommandBufferBuild = CommandBufferBuild (WindowBundle -> CommandPool -> Queue -> Fence -> IO ())

data BoundQueueThread = BoundQueueThread
  {
    boundThreadId :: ThreadId,
    runQueue :: TChan CommandBufferBuild,
    completionVar :: TMVar (Either SomeException ())
  }

data BoundQueueEndedException = BoundQueueEndedException deriving (Eq, Show)

instance Exception BoundQueueEndedException


forkBoundSubmitter :: WindowBundle -> Queue -> IO BoundQueueThread
forkBoundSubmitter wb submitQueue = do
  runQ <- atomically newTChan
  completionV <- newEmptyTMVarIO
  let d = deviceHandle (vulkanDeviceBundle wb)
  let qIndex = auxiliaryQueuesFamilyIndex (vulkanDeviceBundle wb)
  boundThread <- forkOS $
    VK.withCommandPool d (CommandPoolCreateInfo VZ.zero qIndex) Nothing bracket $ \cPool -> do
      VK.withFence d (VK.FenceCreateInfo () VK.FENCE_CREATE_SIGNALED_BIT) Nothing bracket $ \syncFence -> do
        loopResult <- catches
          (boundThreadLoop wb cPool submitQueue syncFence runQ)
          [ -- we expect it to end via getting a BoundQueueEndedException, so that returns a Right
          Handler (\BoundQueueEndedException -> return (Right ())),
          -- any other exception is unexpected and results in a Left
          Handler (\e                        -> return (Left e))
          ]
        atomically (putTMVar completionV loopResult)
  return $ BoundQueueThread boundThread runQ completionV

-- | End the OpenGL thread. Swallows any exceptions in the OpenGL thread.
endBoundSubmitter :: BoundQueueThread -> IO ()
endBoundSubmitter bqt = do
  throwTo (boundThreadId bqt) BoundQueueEndedException
  boundResult <- atomically $ takeTMVar (completionVar bqt)
  case boundResult of
    Left _e -> putStrLn "Error in bound queue thread"
    Right () -> return ()

-- | Code that gets run in the OpenGL bound thread. Really it just runs anything in the queues,
--   preferring to run stuff in the priority queue.
boundThreadLoop :: WindowBundle -> CommandPool -> Queue -> Fence -> TChan CommandBufferBuild -> IO (Either SomeException ())
boundThreadLoop wb cPool vkQ fence runQ = do
  nextTask <- atomically $ do
    t1 <- tryReadTChan runQ
    case t1 of
      Just (CommandBufferBuild t) -> return t
      Nothing -> retry
  print "task"
  nextTask wb cPool vkQ fence
  boundThreadLoop wb cPool vkQ fence runQ

-- | Take some code that builds and submits a command buffer, and run it in the bound submitter thread.
boundCommandBuffer :: BoundQueueThread -> (WindowBundle -> CommandPool -> Queue -> Fence -> IO ()) -> IO ()
boundCommandBuffer bqt !task = atomically $ writeTChan (runQueue bqt) (CommandBufferBuild task)

-- | A wrapper around 'boundCommandBuffer' that handles boilerplate for you. All you need is to fill the command
--   buffer passed into your function. Allocation/deallocation of the command buffer is handled for you, and also
--   waiting for the command buffer to finish is handled for you via a fence and 'TMVar'. There aren't any semaphores
--   used in the submit, however. If you want that you will need to build it yourself using 'boundCommandBuffer'
awaitCommandBuffer :: BoundQueueThread -> (CommandBuffer -> IO a) -> IO a
awaitCommandBuffer bqt !task = do
  resultVar <- newEmptyTMVarIO
  let computation = wrapBoundCommand task resultVar
  -- put in the queue and the thread loop will run it eventually
  atomically $ writeTChan (runQueue bqt) (CommandBufferBuild computation)
  -- wait for the code to run and put the result in resultVar
  --print "waitTMVar"
  atomically $ takeTMVar resultVar

wrapBoundCommand :: (CommandBuffer -> IO a) -> TMVar a -> WindowBundle -> CommandPool -> Queue -> Fence -> IO ()
wrapBoundCommand cmd resultVar wb cPool qew fence = do
  let d = deviceHandle (vulkanDeviceBundle wb)
  cmdBuffers <- VK.allocateCommandBuffers d (VK.CommandBufferAllocateInfo cPool VK.COMMAND_BUFFER_LEVEL_PRIMARY 1)
  let cmdBuffer = cmdBuffers ! 0
  let beginInfo = VK.CommandBufferBeginInfo () VK.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT Nothing
  VK.beginCommandBuffer cmdBuffer beginInfo
  result <- cmd cmdBuffer
  VK.endCommandBuffer cmdBuffer
  VK.resetFences d (fromList [fence])
  let submitInfo = SomeStruct $ SubmitInfo () V.empty V.empty (V.fromList [VK.commandBufferHandle cmdBuffer]) V.empty
  VK.queueSubmit qew (fromList [submitInfo]) fence
  _ <- VK.waitForFences d (fromList [fence]) True maxBound
  VK.freeCommandBuffers d cPool (fromList [cmdBuffer])
  print "putTMVar"
  atomically $ putTMVar resultVar result

-- | Generates a OneShotSubmitter monad that sends stuff to the bound thread
runBoundOneShot :: (LastMember IO effs) => BoundQueueThread -> Eff (OneShotSubmitter ': effs) ~> Eff effs
runBoundOneShot boundQ = interpret $ \(OneShotCommand cmd) -> sendM $ awaitCommandBuffer boundQ cmd
