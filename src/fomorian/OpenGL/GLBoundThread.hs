{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Fomorian.GLBoundThread
Description : Handles forking and running a bound thread to run OpenGL commands.

All OpenGL commands must run in the same thread, which is also the thread that originally created the OpenGL context.
However the GHC runtime normal behavior is to just Haskell tasks on whatever thread is available, possibly
switching OS threads as needed. To prevent this we need to create a 'bound thread' which will stick to one OS thread.
Then all OpenGL code gets run on this thread.

Also, there are multiple tasks that need to call OpenGL for resource loading and unloading, as well as for rendering.
To allow this the OpenGL bound thread runs in a simple loop where it runs functions submitted by other tasks.
-}
module Fomorian.OpenGL.GLBoundThread where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception

data GLEndedException = GLEndedException deriving (Eq, Show)

instance Exception GLEndedException

-- | Record describing a bound OpenGL thread. Contains channels where you can submit data.
data BoundGLThread w = BoundGLThread {
    boundID :: ThreadId
  , windowValue :: TMVar w
  , priorityQueue :: TChan (IO ())
  , defaultQueue :: TChan (IO ())
  , completionVar :: TMVar (Either SomeException ())
}

-- | Starts up the OpenGL bound thread, using the provided parameters to initialize and cleanup the OpenGL context. Use the returned
--   'BoundGLThread' to submit stuff to run.
forkBoundGLThread :: IO a -> (a -> IO ()) -> IO (BoundGLThread a)
forkBoundGLThread initwin cleanup = do
  windowVar <- atomically newEmptyTMVar
  queue1 <- atomically newTChan
  queue2 <- atomically newTChan
  completionSync <- atomically newEmptyTMVar
  boundThread <- forkOS $ do
    loopResult <- catches
      (bracket initwin cleanup (\w -> do
                                  atomically $ putTMVar windowVar w
                                  glThreadLoop w queue1 queue2))
      [ -- we expect it to end via getting a GLEndedException, so that returns a Right
       Handler (\GLEndedException -> return (Right ())),
       -- any other exception is unexpected and results in a Left
       Handler (\e                -> return (Left e))
      ]
    atomically (putTMVar completionSync loopResult)
  return (BoundGLThread boundThread windowVar queue1 queue2 completionSync) 

-- | End the OpenGL thread. Swallows any exceptions in the OpenGL thread.
endBoundGLThread :: BoundGLThread a -> IO ()
endBoundGLThread bgl = do
  throwTo (boundID bgl) GLEndedException
  boundResult <- atomically $ takeTMVar (completionVar bgl)
  case boundResult of
    Left e -> putStrLn "Error in bound GL thread"
    Right () -> return ()

-- | Code that gets run in the OpenGL bound thread. Really it just runs anything in the queues,
--   preferring to run stuff in the priority queue.
glThreadLoop :: a -> TChan (IO()) -> TChan (IO ()) -> IO (Either SomeException ())
glThreadLoop w priorityQ stdQ = do
  nextTask <- atomically $ do
    t1 <- tryReadTChan priorityQ
    case t1 of
      Just t -> return t
      Nothing -> do
        t2 <- tryReadTChan stdQ
        case t2 of
          Just t -> return t
          Nothing -> retry
  nextTask
  glThreadLoop w priorityQ stdQ


submitPriorityGLTask :: BoundGLThread w -> IO () -> IO ()
submitPriorityGLTask bgl !task = atomically $ writeTChan (priorityQueue bgl) task

waitForPriorityGLTask :: BoundGLThread w -> IO () -> IO ()
waitForPriorityGLTask bgl !task = do
  resultVar <- atomically newEmptyTMVar
  let computation = catch @SomeException
                        (do task
                            atomically (putTMVar resultVar ())
                            return ())
                        (\e -> atomically (putTMVar resultVar ()))
  -- put in the queue and the thread loop will run it eventually
  atomically $ writeTChan (priorityQueue bgl) computation
  -- wait for the code to run and put the result in resultVar
  atomically $ takeTMVar resultVar


submitGLTask :: BoundGLThread w -> IO () -> IO ()
submitGLTask bgl !task = atomically $ writeTChan (defaultQueue bgl) task

-- | Submit code to run in the GL thread. If an exception happens in the GL thread this returns a 'Left'.
--   If you want the GL exception to propagate back to the original submitter use 'submitGLComputationThrow'
submitGLComputation :: BoundGLThread w -> IO a -> IO (Either SomeException a)
submitGLComputation bgl !compute = do
  resultVar <- atomically newEmptyTMVar
  let computeResult = catch
                        (do result <- compute
                            atomically (putTMVar resultVar (Right result))
                            return ())
                        (\e -> atomically (putTMVar resultVar (Left e)))
  -- put in the queue and the thread loop will run it eventually
  atomically $ writeTChan (defaultQueue bgl) computeResult
  -- wait for the code to run and put the result in resultVar
  atomically $ takeTMVar resultVar

-- | Run 'submitGLComputation' and if an exception happens in the GL thread, throw an exception in the original calling thread.
submitGLComputationThrow :: BoundGLThread w -> IO a -> IO a
submitGLComputationThrow bgl !compute = do
  result <- submitGLComputation bgl compute
  case result of
    Left ex -> throw ex
    Right val -> return val

