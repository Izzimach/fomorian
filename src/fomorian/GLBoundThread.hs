{-# LANGUAGE DataKinds #-}

module Fomorian.GLBoundThread where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception


data GLEndedException = GLEndedException deriving (Eq, Show)

instance Exception GLEndedException


data BoundGLThread = BoundGLThread {
    boundID :: ThreadId
  , priorityQueue :: TChan (IO ())
  , defaultQueue :: TChan (IO ())
  , completionVar :: TMVar (Either SomeException ())
}

forkBoundGLThread :: IO a -> (a -> IO ()) -> IO BoundGLThread
forkBoundGLThread initwin cleanup = do
  queue1 <- atomically newTChan
  queue2 <- atomically newTChan
  completionSync <- atomically newEmptyTMVar
  boundThread <- forkOS $ do
    loopResult <- catches
      (bracket initwin cleanup (\w -> glThreadLoop w queue1 queue2))
      [ -- we expect it to end via getting a GLEndedException, so that returns a Right
       Handler (\GLEndedException -> return (Right ())),
       -- any other exception is unexpected and results in a Left
       Handler (\e                -> return (Left e))
      ]
    atomically (putTMVar completionSync loopResult)
  return (BoundGLThread boundThread queue1 queue2 completionSync) 


glThreadLoop :: a -> TChan (IO()) -> TChan (IO ()) -> IO (Either SomeException ())
glThreadLoop w q1 q2 = do
  nextTask <- atomically $ do
    t1 <- tryReadTChan q1
    case t1 of
      Just t -> return t
      Nothing -> do
        t2 <- tryReadTChan q2
        case t2 of
          Just t -> return t
          Nothing -> retry
  nextTask
  glThreadLoop w q1 q2

submitPriorityGLTask :: BoundGLThread -> IO () -> IO ()
submitPriorityGLTask bgl task = atomically $ writeTChan (priorityQueue bgl) task

submitGLTask :: BoundGLThread -> IO () -> IO ()
submitGLTask bgl task = atomically $ writeTChan (defaultQueue bgl) task

submitGLComputation :: BoundGLThread -> IO a -> IO (Either SomeException a)
submitGLComputation bgl compute = do
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
