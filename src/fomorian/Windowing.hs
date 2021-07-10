{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}



-- 
module Fomorian.Windowing where

import qualified Graphics.UI.GLFW as GLFW

import qualified Graphics.Rendering.OpenGL as GL
import Control.Concurrent
import Control.Concurrent.STM

import Fomorian.GLBoundThread

data OpenGLBinding = UseOpenGL | NoOpenGL deriving (Eq, Show)

data WindowInitData = WindowInitData
  {
    width :: Int,
    height:: Int,
    title :: String,
    openGLContext :: OpenGLBinding
  } deriving (Eq, Show)


-- | Create a window, maybe bind an OpenGL context to that window depending on
--   the value of 'openGLContext' in 'WindowInitData'. Returns the window handle
initWindow :: WindowInitData -> IO GLFW.Window
initWindow (WindowInitData w h t o) = do
  _ <- GLFW.init
  GLFW.defaultWindowHints
  if (o == NoOpenGL) then
    GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
  else
    return ()
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 4)
  GLFW.windowHint (GLFW.WindowHint'Resizable True)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  win <- GLFW.createWindow w h t Nothing Nothing
  case win of
    Nothing -> fail "Error initializing window"
    Just windowID ->
      if (o == UseOpenGL) then
        do GLFW.makeContextCurrent win
           GLFW.swapInterval 1      -- should wait for vsync, set to 0 to not wait
           return windowID
      else
        do return windowID




-- | Close a currently-active window.
terminateWindow :: GLFW.Window -> IO ()
terminateWindow windowID = do
  GLFW.destroyWindow windowID
  GLFW.terminate


-- | A quick utility function to test code in GHCI. This wraps your code with a proper setup/teardown of a valid OpenGL context.
--   Sometimes it is useful to test OpenGL code by running it in the GHCI REPL. However, just calling random OpenGL functions in GHCI will fail
--   because you need an active and bound OpenGL context. This functions provides that: @runWithGL <your OpenGL code>@
--   You pass in a monad that represents your the OpenGL code.
--   Them 'runWithGL' opens a window, binds an OpenGL context, and runs your code. The window is then terminated and whatever your code returned is
--   passed out of the monad.
--
--   example :: You want to run 'genObjectNames' to see what it returns. Wrap it in 'runWithGL':
--              @runWithGL $ ((genObjectNames 1) :: IO [BufferObject])@. This creates a 'BufferObject' and returns the id. Note that the actual 'BufferObject' tracked
--              by OpenGL will have
--              been destroyed by the time you see it, since the window is killed after your code is run. What you will see is the ID that was used while the window was open.
--
runWithGL :: (BoundGLThread GLFW.Window -> IO a) -> IO a
runWithGL go =
  do let initwin = initWindow (WindowInitData 600 400 "Test" UseOpenGL)
     glThreadData <- forkBoundGLThread initwin terminateWindow
     goResult <- go glThreadData
     throwTo (boundID glThreadData) GLEndedException
     glThreadResult <- atomically $ takeTMVar (completionVar glThreadData)
     case glThreadResult of
       Right () -> putStrLn "GLThread shutdown successfully"
       Left e -> putStrLn $ "GLThread shutdown exception: " ++  show e
     return goResult

testRun :: BoundGLThread GLFW.Window -> IO ()
testRun glThread = do
  submitPriorityGLTask glThread (putStrLn "Gl Priority")
  submitGLTask glThread (putStrLn "Gl default")
  let computation = do threadDelay 50000
                       GL.createShader GL.VertexShader
  computeResult <- submitGLComputation glThread computation
  case computeResult of
    Left e -> putStrLn $ "compute error " ++ show e
    Right r -> do
      putStrLn $ "compute Result " ++ show computeResult
      _ <- submitGLComputation glThread $ GL.deleteObjectName r
      return ()
  return ()
