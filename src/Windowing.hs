{-# LANGUAGE DataKinds, PolyKinds, 

TypeOperators,
TypeFamilies,
FlexibleContexts, 
FlexibleInstances, 
NoMonomorphismRestriction,

GADTs, TypeSynonymInstances, TemplateHaskell, OverloadedLabels,

StandaloneDeriving,
RankNTypes
#-}

-- 
module Windowing where

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU
import qualified Graphics.UI.GLFW as GLFW

import SceneResources
import SceneNode

import Linear
import Data.Vinyl
import Data.Word (Word32)
import Graphics.VinylGL

import Data.IORef
import Data.Maybe (mapMaybe)
import Control.Monad
import Control.Monad.State
import Control.Lens ((^.), (.~), (%~))
import Control.Exception

type AppInfo = FieldRec '[  '("window", GLFW.Window),
                            '("windowSize", (Int,Int)),
                            '("resources", ResourceMap),
                            '("curTime",Float)]


resizeWindow :: IORef AppInfo -> GLFW.WindowSizeCallback
resizeWindow s = \_ w h -> windowResizeEvent s w h
    
windowResizeEvent :: IORef AppInfo -> Int -> Int -> IO ()
windowResizeEvent s w h = do
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  modifyIORef' s $ rputf #windowSize (w,h)

initWindow :: (Int,Int,String) -> IO GLFW.Window
initWindow (w,h,title) = do
  GLFW.init
  GLFW.defaultWindowHints
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 4)
  GLFW.windowHint (GLFW.WindowHint'Resizable True)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  Just win <- GLFW.createWindow w h title Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  return win

initAppState :: (Int,Int,String) -> GLFW.Window -> IO (IORef AppInfo)
initAppState (w,h,title) win = do
  defaultVAO <- fmap head (genObjectNames 1)
  bindVertexArrayObject $= Just defaultVAO
  GLU.printErrorMsg "bindVAO"
  appIORef <- newIORef $ (#window =: win)
                      :& (#windowSize =: (w,h))
                      :& (#resources =: emptyResourceMap)
                      :& (#curTime =: (0 :: Float))
                      :& RNil
  GLFW.setWindowSizeCallback win (Just $ resizeWindow appIORef)
  return appIORef

  
terminateWindow :: GLFW.Window -> IO ()
terminateWindow win = do
  GLFW.destroyWindow win
  GLFW.terminate

shouldEndProgram :: GLFW.Window -> IO Bool
shouldEndProgram win = do
  p <- GLFW.getKey win GLFW.Key'Escape
  GLFW.pollEvents
  windowKill <- GLFW.windowShouldClose win
  return (p == GLFW.KeyState'Pressed ||  windowKill)

renderLoop :: (NeedsResources s, Drawable s (FieldRec fr), FrameConstraints s  (FieldRec fr)) => IORef AppInfo -> (AppInfo -> SceneGraph s (FieldRec fr)) -> (AppInfo -> PerFrameData fr) -> IO ()
renderLoop appref buildScene genRP = loop
  where
    loop = do
        appstate <- readIORef appref
        let win = rvalf #window appstate
        let scene = buildScene appstate
        let resources = (rvalf #resources appstate)
        new_resources <- syncResourcesForScene scene resources
        let new_appstate = (rputf #resources new_resources $ appstate)
        let frame_data = genRP new_appstate
        renderApp new_resources scene frame_data
        writeIORef appref new_appstate

        GLFW.swapBuffers win
        shouldClose <- shouldEndProgram win
        unless shouldClose loop

renderApp :: (Drawable s (FieldRec fr), FrameConstraints s (FieldRec fr)) => ResourceMap -> SceneGraph s (FieldRec fr) -> PerFrameData fr -> IO ()
renderApp resources scene framedata = do
  GL.clearColor $= Color4 0 0 0 0
  GL.clear [GL.ColorBuffer]
  renderresult <- try $ renderScene scene framedata resources
  case renderresult of
    Left e -> do
      putStrLn $ displayException (e :: SomeException)
    Right () -> return ()
  