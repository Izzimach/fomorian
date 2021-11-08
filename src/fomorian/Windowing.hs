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


{-
Functions to create/destroy a window and (optionally) create and bind an OpenGL context.
-} 
module Fomorian.Windowing where

import Control.Monad ( when )

import qualified Graphics.UI.GLFW as GLFW

-- | Use this to dictate whether or not you want an OpenGL context bound when initializing the window.
data OpenGLBinding = UseOpenGL | NoOpenGL deriving (Eq, Show)

data WindowInitData = WindowInitData
  {
    width :: Int,
    height:: Int,
    title :: String,
    openGLContext :: OpenGLBinding
  } deriving (Eq, Show)


-- | Create a window using GLFW.  Also maybe bind an OpenGL context to that window depending on
--   the value of 'openGLContext' in 'WindowInitData'. Returns the window handle
initWindow :: WindowInitData -> IO GLFW.Window
initWindow (WindowInitData w h t o) = do
  _ <- GLFW.init
  GLFW.defaultWindowHints
  when (o == NoOpenGL) $ GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 4)
  GLFW.windowHint (GLFW.WindowHint'Resizable True)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  win <- GLFW.createWindow w h t Nothing Nothing
  case win of
    Nothing -> fail "Error initializing window"
    Just windowID -> do
      when (o == UseOpenGL) $ do
        GLFW.makeContextCurrent win
        GLFW.swapInterval 1      -- should wait for vsync, set to 0 to not wait
      return windowID




-- | Close a currently-active window and clean up GLFW stuff. This doesn't actually end the program, just clean up any GLFW resources created with 'GLFW.init'
terminateWindow :: GLFW.Window -> IO ()
terminateWindow windowID = do
  GLFW.destroyWindow windowID
  GLFW.terminate

