{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs, TypeSynonymInstances, TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE OverloadedLabels #-}

module Sample where

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU
import qualified Graphics.UI.GLFW as GLFW
import Linear
import Data.Vinyl
import Data.Word (Word32)
import Graphics.VinylGL

import Data.IORef
import Data.Maybe (mapMaybe)
import Control.Monad
import Control.Lens ((^.), (.~))
import Control.Exception
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import System.FilePath ((</>))

import SceneNode
import SceneResources


type AppInfo = FieldRec '[  '("projectionMatrix", M44 GLfloat),
                            '("resources", ResourceMap), 
                            '("scene",SceneGraph) ]

buildScene :: IO SceneGraph
buildScene = let snode = (  
                         (#shader =: "linez")
                      :& (#shaderParameters =: ((#tex =: 0) :& RNil) )
                      :& (#vertexBuffers =: [
                          V2Data [V2 10 10, V2 100 10, V2 10 100, V2 100 100],
                          IndexData [0,1,2, 2,1,3]
                          ]
                         )
                      :& (#textures =: ["lollipopGreen.png"])
                      :& (#children =: [])
                      :& RNil
                      ) :: (SceneNodeR (FieldRec '[ '("tex", GLint)]))
             in return $ SG snode
               

setProjectionMatrix :: IORef AppInfo -> Int -> Int -> IO ()
setProjectionMatrix s w h = do
  modifyIORef' s f
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  where
    w1 = 1.0 / (fromIntegral w)
    h1 = 1.0 / (fromIntegral h)
    scaleMatrix x y z = V4 (V4 x 0 0 0)
                           (V4 0 y 0 0)
                           (V4 0 0 z 0)
                           (V4 0 0 0 1)
    translationMatrix x y z = V4 (V4 1 0 0 x)
                                 (V4 0 1 0 y)
                                 (V4 0 0 1 z)
                                 (V4 0 0 0 1)
    finalMatrix = ((translationMatrix (-1) (-1) 0) !*! (scaleMatrix (w1*2) (h1*2) 1))
    f :: AppInfo -> AppInfo
    f app = rputf #projectionMatrix finalMatrix app

resizeWindow :: IORef AppInfo -> GLFW.WindowSizeCallback
resizeWindow s = \_ w h -> setProjectionMatrix s w h


initWindow :: IO (GLFW.Window, IORef AppInfo)
initWindow = do
  GLFW.init
  GLFW.defaultWindowHints
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 4)
  GLFW.windowHint (GLFW.WindowHint'Resizable False)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  Just win <- GLFW.createWindow 400 400 "Title" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  appIORef <- initAppState
  GLFW.setWindowSizeCallback win (Just $ resizeWindow appIORef)
  setProjectionMatrix appIORef 400 400
  return (win, appIORef)

initAppState :: IO (IORef AppInfo)
initAppState = do
  defaultVAO <- fmap head (genObjectNames 1)
  bindVertexArrayObject $= Just defaultVAO
  GLU.printErrorMsg "bindVAO"
  sceneGraph <- buildScene
  resources <- loadResourcesForGraph sceneGraph
  appIORef <- newIORef $ (#projectionMatrix =: identity)
                      :& (#resources =: resources)
                      :& (#scene =: sceneGraph)
                      :& RNil
  return appIORef

  
terminateWindow :: GLFW.Window -> IO ()
terminateWindow win = do
  GLFW.destroyWindow win
  GLFW.terminate


main :: IO ()
main = do
  (win, appIORef) <- initWindow
  renderloop win appIORef
  terminateWindow win

renderloop :: HasGetter t AppInfo => GLFW.Window -> t -> IO ()
renderloop win appstateref = loop
  where
    loop = do
        appstate <- get appstateref
        renderApp appstate
        GLFW.swapBuffers win
        p <- GLFW.getKey win GLFW.Key'Escape
        unless (p == GLFW.KeyState'Pressed) $ do
          GLFW.pollEvents
          windowOpen <- GLFW.windowShouldClose win
          unless (windowOpen) $ loop

renderApp :: AppInfo -> IO ()
renderApp appstate = do
  GL.clearColor $= Color4 0 0 0 0
  GL.clear [GL.ColorBuffer]
  let resourceMap = rvalf #resources appstate
  let renderparams = (#transformMatrix =: (rvalf #projectionMatrix appstate))
                  :& (#tex =: 0)
                  :& RNil
  let scene = rvalf #scene appstate
  renderresult <- try $ renderScene scene renderparams resourceMap
  case renderresult of
    Left e -> do
      putStrLn $ displayException (e :: SomeException)
    Right () -> return ()

