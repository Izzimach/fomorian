{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Fomorian.SimpleApp where

import Linear

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Control.Exception (try, SomeException, displayException, bracket)

import Data.Row
import Data.Row.Records
import qualified Data.Map as M
import Data.IORef

import Fomorian.SceneNode
import Fomorian.SceneResources
import Fomorian.OpenGLResources
import Fomorian.Windowing
import Fomorian.OpenGLCommand

import LoadUnload

import Control.Monad (unless)


type OuterAppRow = ("window"     .== GLFW.Window .+
                    "windowSize" .== (Int,Int)   .+
                    "resources"  .== LoadedResources (DataSource GLDataSourceTypes) (Resource GLResourceTypes) .+
                    "curTime"    .== Float       .+
                    "shouldTerminate" .== Bool)


-- | Parameters stored as App state that persists between frames.
type AppInfo = Rec OuterAppRow


resizeWindow :: IORef AppInfo -> GLFW.WindowSizeCallback
resizeWindow ref = \_ w h -> windowResizeEvent ref w h


windowResizeEvent :: IORef AppInfo -> Int -> Int -> IO ()
windowResizeEvent ref w h = do
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  modifyIORef' ref $ \r -> update #windowSize (w,h) r


-- | Initializes the app state and OpenGL. Call after you open the window.
initAppState :: WindowInitData -> GLFW.Window -> IO (IORef AppInfo)
initAppState (WindowInitData w h _ _) win = do
  let initialAppState =    (#window .== win)
                        .+ (#windowSize .== (w,h))
                        .+ (#resources .== noLoadedResources)
                        .+ (#curTime .== (0 :: Float))
                        .+ (#shouldTerminate .== False)
  appIORef <- newIORef initialAppState
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  GLFW.setWindowSizeCallback win (Just $ resizeWindow appIORef)
  return appIORef


-- | Checks for end events. Specifically any close events from GLFW or hitting the escapse key.
shouldEndProgram :: GLFW.Window -> IO Bool
shouldEndProgram win = do
  p <- GLFW.getKey win GLFW.Key'Escape
  GLFW.pollEvents
  windowKill <- GLFW.windowShouldClose win
  let shouldTerminate = (p == GLFW.KeyState'Pressed ||  windowKill)
  return shouldTerminate


-- | Given a scene graph, draw a single frame on an OpenGL canvas.
renderOneFrame :: SceneGraph OpenGLCommand dr -> Rec dr -> IO ()
renderOneFrame scene frameData = do
  GL.clearColor $= Color4 0 0.5 0.5 1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  depthFunc $= Just Less
  cullFace $= Just Front
  renderresult <- try $ openGLgo scene frameData
  case renderresult of
    Left e   -> putStrLn $ displayException (e :: SomeException)
    Right () -> return ()



-- | Runs a render loop by generating a scene graph and frame parameters from app state.
renderLoop :: IORef AppInfo -> (AppInfo -> SceneGraph OpenGLTarget dr) -> (AppInfo -> Rec dr) -> IO ()
renderLoop appref buildScene genFD = loop
  where
    loop = do
      appstate <- readIORef appref
      let win = appstate .! #window
      let sceneTarget = buildScene appstate

      -- generate new resources list and time
      resources' <- loadOpenGLResourcesScene sceneTarget (appstate .! #resources)
      let curTime' = (appstate .! #curTime) + 0.016

      -- put these new values back into the app state
      let appstate' = update #curTime curTime' $ 
                      update #resources resources' $
                      appstate
      writeIORef appref appstate'

      -- generate the draw commands
      let rawResources = OpenGLResources $ M.map value (resourceMap resources')
      let sceneCommand = oglToCommand rawResources sceneTarget
      let frameData = genFD appstate'
      renderOneFrame sceneCommand frameData

      GLFW.swapBuffers win
      shouldTerminate <- shouldEndProgram win
      unless shouldTerminate loop


type TopLevel3DRow = ("modelMatrix" .== (M44 Float) .+ "viewMatrix" .== (M44 Float) .+ "projectionMatrix" .== (M44 Float) .+ "curTime" .== Float .+ "windowX" .== Integer .+ "windowY" .== Integer)

-- | Given app state generates some default frame parameters
simpleAppRenderParams :: AppInfo -> Rec TopLevel3DRow
simpleAppRenderParams appstate =
  let t     = appstate .! #curTime
      (w,h) = appstate .! #windowSize
  in   (#modelMatrix .== (identity :: M44 Float)) .+
       (#viewMatrix .== (identity :: M44 Float)) .+
       (#projectionMatrix .== (identity :: M44 Float)) .+
       (#curTime .== t) .+
       (#windowX .== fromIntegral w) .+
       (#windowY .== fromIntegral h)



-- | A basic app that just runs a render function over and over.
simpleApp :: (Int, Int) -> (AppInfo -> SceneGraph OpenGLTarget TopLevel3DRow) -> IO ()
simpleApp (w,h) renderFunc = do
  let initData = WindowInitData w h "Haskell App" UseOpenGL
  let initfunc = initWindow initData
  let endfunc  = \win -> terminateWindow win
  let loopfunc = \win -> do
                           appdata <- initAppState initData win
                           renderLoop appdata renderFunc simpleAppRenderParams
  bracket initfunc endfunc loopfunc
