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
module Fomorian.Windowing 
(initWindow,
 initAppState,
 terminateWindow,
 renderLoop,
 AppInfo(..),
 AppAction(..)) where

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU
import qualified Graphics.UI.GLFW as GLFW

import Fomorian.SceneResources
import Fomorian.SceneNode
import Fomorian.Common

import Linear
import Data.Vinyl
import Data.Word (Word32)
import qualified Data.Constraint as DC
import Graphics.VinylGL

import Data.IORef
import Data.Maybe (mapMaybe)
import Control.Monad
import Control.Monad.State
import Control.Lens ( (^.), (.~), (%~) )
import Control.Exception

type AppInfo = FieldRec '[ '("window", GLFW.Window),
                           '("windowSize", (Int,Int)),
                           '("resources", ResourceMap),
                           '("curTime", Float) ]

data AppAction = NextFrame | EndApp 
  deriving (Eq, Show)


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
  GLFW.swapInterval 1      -- should wait for vsync, set to 0 to not wait
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
  openGLgo scene framedata resources
  return ()      


renderLoop ::
  IORef AppInfo -> 
  (AppInfo -> SceneGraph (FieldRec '[]) TopWindowFrameParams DrawGL) ->
  (AppInfo -> TopWindowFrameParams) ->
  (AppInfo -> AppAction) -> 
  IO ()
renderLoop appref buildScene genRP doNext = loop
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
        externalClose <- shouldEndProgram win
        let shouldClose = (doNext new_appstate == EndApp) || externalClose
        unless shouldClose loop

