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

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU
import qualified Graphics.UI.GLFW as GLFW

import Linear
import Data.Row
import Data.Word (Word32)
import qualified Data.Constraint as DC

import Data.IORef
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Control.Exception



data WindowInitData = WindowInitData
  {
    width :: Int,
    height:: Int,
    title :: String
  } deriving (Eq, Show)


-- | Create a window and bind an OpenGL context to that window. Returns the window handle
initWindowGL :: WindowInitData -> IO GLFW.Window
initWindowGL (WindowInitData w h t) = do
  GLFW.init
  GLFW.defaultWindowHints
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 4)
  GLFW.windowHint (GLFW.WindowHint'Resizable True)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  win <- GLFW.createWindow w h t Nothing Nothing
  case win of
    Nothing -> fail "Error initializing window"
    Just windowID ->
      do GLFW.makeContextCurrent win
         GLFW.swapInterval 1      -- should wait for vsync, set to 0 to not wait
         return windowID

-- | Close a currently-active window.
terminateWindowGL :: GLFW.Window -> IO ()
terminateWindowGL windowID = do
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
runWithGL :: (IO a) -> IO a
runWithGL go =
  do w <- initWindowGL (WindowInitData 600 400 "Test")
     ret <- go
     terminateWindowGL w
     return ret

