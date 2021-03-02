{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Fomorian.SimpleApp where

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU
import qualified Graphics.UI.GLFW as GLFW

import Data.Row
import Data.Row.Records
import Data.IORef

import Fomorian.Windowing

type OuterAppRow = ("window"     .== GLFW.Window .+
                    "windowSize" .== (Int,Int)   .+
--                    "resources" .== ResourceMap .+
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
                        -- .+ (#resources .== emptyResourceMap)
                        .+ (#curTime .== (0 :: Float))
                        .+ (#shouldTerminate .== False)
    appIORef <- newIORef initialAppState

    GLFW.setWindowSizeCallback win (Just $ resizeWindow appIORef)

    return appIORef


-- | Updates the 'shouldTerminate' field to true if the user presses escape or some other signal was sent from GLFW to close the window.
shouldEndProgram :: (HasType "window" GLFW.Window r, HasType "shouldTerminate" Bool r) => Rec r -> IO (Rec r)
shouldEndProgram r = do
  let win = r .! #window
  p <- GLFW.getKey win GLFW.Key'Escape
  GLFW.pollEvents
  windowKill <- GLFW.windowShouldClose win
  let shouldTerminate = (p == GLFW.KeyState'Pressed ||  windowKill)
  return (update #shouldTerminate shouldTerminate r)

{-
renderApp ::
  ResourceMap ->
  SceneGraph (FieldRec '[]) TopWindowFrameParams DrawGL ->
  TopWindowFrameParams ->
  IO ()
renderApp resources scene windowparams = do
  let framedata = FrameData RNil windowparams DC.Dict
  GL.clearColor $= Color4 0.1 0.1 0.1 1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  depthFunc $= Just Less
  cullFace $= Just Front
  renderresult <- try $ openGLgo scene framedata resources
  case renderresult of
    Left e   -> putStrLn $ displayException (e :: SomeException)
    Right () -> return ()
      


renderLoop ::
  IORef AppInfo -> 
  (AppInfo -> SceneGraph (Rec Empty) TopWindowFrameParams DrawGL) ->
  (AppInfo -> TopWindowFrameParams) ->
  IO ()
renderLoop appref buildScene genRP = loop
  where
    loop = do
      appstate <- readIORef appref
      let win = rvalf #window appstate
      let resources = rvalf #resources appstate
      let needresources = oglResourcesScene $ buildScene appstate
      new_resources <- loadResources needresources resources
      let bumpTime = (rlensf #curTime) %~ (+0.016)
      let new_appstate = bumpTime . (rputf #resources new_resources) $ appstate

      let frame_data = genRP new_appstate
      let scene = buildScene appstate
      renderApp new_resources scene frame_data
      writeIORef appref new_appstate

      GLFW.swapBuffers win
      shouldClose <- shouldEndProgram win
      unless shouldClose loop
-}
