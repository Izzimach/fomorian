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
import Data.IORef

import Fomorian.SceneNode
import Fomorian.SceneResources
import Fomorian.Windowing
import Fomorian.OpenGLCommand
import Control.Monad (unless)

-- | Parameters stored as App state that persists between frames.
type OuterAppRow = ("window"     .== GLFW.Window .+
                    "windowSize" .== (Int,Int)   .+
                    "resources"  .== Resources GLDataSource GLResourceRecord .+
                    "curTime"    .== Float       .+
                    "shouldTerminate" .== Bool)

type AppInfo = Rec OuterAppRow


resizeWindow :: IORef AppInfo -> GLFW.WindowSizeCallback
resizeWindow ref = \_ w h -> windowResizeEvent ref w h
    
windowResizeEvent :: IORef AppInfo -> Int -> Int -> IO ()
windowResizeEvent ref w h =
  do
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    modifyIORef' ref $ \r -> update #windowSize (w,h) r



initAppState :: WindowInitData -> GLFW.Window -> IO (IORef AppInfo)
initAppState (WindowInitData w h _) win =
  do
    defaultVAO <- fmap head (genObjectNames 1)
    bindVertexArrayObject $= Just defaultVAO

    let initialAppState = (#window .== win)
                        .+ (#windowSize .== (w,h))
                        .+ (#resources .== emptyResources)
                        .+ (#curTime .== (0 :: Float))
                        .+ (#shouldTerminate .== False)
    appIORef <- newIORef initialAppState
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

    GLFW.setWindowSizeCallback win (Just $ resizeWindow appIORef)

    return appIORef


-- | Updates the 'shouldTerminate' field to true if the user presses escape or some other signal was sent from GLFW to close the window.
shouldEndProgram :: (HasType "window" GLFW.Window r) => Rec r -> IO Bool
shouldEndProgram r = do
  let win = r .! #window
  p <- GLFW.getKey win GLFW.Key'Escape
  GLFW.pollEvents
  windowKill <- GLFW.windowShouldClose win
  let shouldTerminate = (p == GLFW.KeyState'Pressed ||  windowKill)
  return shouldTerminate

-- | Given a scene graph, draw a single frame on an OpenGL canvas.
renderOneFrame :: SceneGraph r OpenGLCommand -> Rec r -> IO ()
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
renderLoop :: IORef AppInfo -> (AppInfo -> SceneGraph r OpenGLTarget ) -> (AppInfo -> Rec r) -> IO ()
renderLoop appref buildScene genFD = loop
  where
    loop = do
      appstate <- readIORef appref
      let win = appstate .! #window
      let sceneTarget = buildScene appstate

      -- generate new resources list and time and put them into the appstate
      resources' <- loadResourcesScene sceneTarget (appstate .! #resources)
      let curTime' = (appstate .! #curTime) + 0.016
      let appstate' = update #curTime curTime' $ 
                      update #resources resources' $
                      appstate

      writeIORef appref appstate'

      -- generate the draw commands
      let sceneCommand = oglToCommand resources' sceneTarget
      let frameData = genFD appstate'
      renderOneFrame sceneCommand frameData

      GLFW.swapBuffers win
      shouldTerminate <- shouldEndProgram appstate'
      unless shouldTerminate loop


type TopLevel2DRow = ("modelViewMatrix" .== (M44 Float) .+ "projectionMatrix" .== (M44 Float) .+ "curTime" .== Float .+ "windowX" .== Integer .+ "windowY" .== Integer)

-- | Given app state generates some default frame parameters
simpleAppRenderParams :: AppInfo -> Rec TopLevel2DRow
simpleAppRenderParams appstate =
  let t     = appstate .! #curTime
      (w,h) = appstate .! #windowSize
  in   (#modelViewMatrix .== (identity :: M44 Float)) .+
       (#projectionMatrix .== (identity :: M44 Float)) .+
       (#curTime .== t) .+
       (#windowX .== fromIntegral w) .+
       (#windowY .== fromIntegral h)


-- | A basic app that just runs a render function over and over.
simpleApp :: (Int, Int) -> (AppInfo -> SceneGraph TopLevel2DRow OpenGLTarget) -> IO ()
simpleApp (w,h) renderFunc = do
  let initData = WindowInitData w h "Haskell App"
  let initfunc = initWindowGL initData
  let endfunc  = \win -> terminateWindowGL win
  let loopfunc = \win -> do
                           appdata <- initAppState initData win
                           renderLoop appdata renderFunc simpleAppRenderParams
  bracket initfunc endfunc loopfunc
