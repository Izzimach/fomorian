{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Fomorian.OpenGL.PlatformRenderer where

import Control.Exception
import Control.Concurrent.STM

import Data.IORef
import Data.Row
import Data.Map (Map)
import Data.Text (Text)

import Linear

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import STMLoader.LoadUnload
import STMLoader.AsyncLoader

import Fomorian.Windowing as W
import Fomorian.SceneNode
import Fomorian.SceneResources

import Fomorian.OpenGL.GLBoundThread
import Fomorian.OpenGL.OpenGLResources
import Fomorian.OpenGL.OpenGLCommand

import Fomorian.PlatformRenderer

-- Code to fire up the loader and bound thread, and then route scene processing through the various parts.
--
-- For a diagram of threads and data flow look at OpenGLFlow.svg

type ErrorMessage = String
type GLResourceCallbacks = LoadUnloadCallbacks (DataSource GLDataSourceTypes) (Resource GLResourceTypes) ErrorMessage

data OpenGLRendererState =
  OpenGLRendererState {
    rendererBoundThread :: BoundGLThread GLFW.Window,
    rendererLoader :: AsyncLoader (DataSource GLDataSourceTypes) (Resource GLResourceTypes) ErrorMessage,
    openGLStats :: IORef RenderStats
  }

-- | Given a bound OpenGL thread 'BoundGLThread' this will generate callbacks that transfer control to the OpenGL thread
--   for loading and unloading. We try to force the values using 'seq' to make thunk evaluation
--   take place in the worker thread and not the OpenGL thread. There is only one OpenGL thread so we don't want to
--   load it down with extra work evaluating thunks.
threadedLoaderGLCallbacks :: Map Text BasicResource -> BoundGLThread w -> GLResourceCallbacks
threadedLoaderGLCallbacks prebuilt glb =
  LoadUnloadCallbacks {
    loadResource     = \l deps -> l `seq` deps `seq` submitGLComputationThrow glb (loadGLResource prebuilt l deps),
    unloadResource   = \res -> submitGLComputationThrow glb (unloadGLResource res),
    findDependencies = computeGLDependencies,
    processException = const Drop
    }

resizeWindow :: IORef RenderStats -> GLFW.WindowSizeCallback
resizeWindow rref = \_ w h -> windowResizeEvent w h
  where
    windowResizeEvent :: Int -> Int -> IO ()
    windowResizeEvent w h = do
      modifyIORef' rref $ \r -> r { windowSize = (V2 w h) }

openGLWrapRenderLoop :: Map Text BasicResource -> (Int, Int) -> (OpenGLRendererState -> IO ()) -> IO ()
openGLWrapRenderLoop prebuiltResources (w,h) wrapped = bracket startGL endGL wrapped
  where
    startGL = do
      let initData = WindowInitData w h "OpenGL Fomorian" UseOpenGL
      glThread <- forkBoundGLThread (W.initWindow initData) terminateWindow

      -- spin up the multithreaded loader, routing loads/unloads to the bound GL thread
      let asyncConfig = AsyncLoaderConfig 2 simpleThreadWrapper simpleThreadWrapper
      let loaderCallbacks = threadedLoaderGLCallbacks prebuiltResources glThread
      mtLoader <- startAsyncLoader asyncConfig loaderCallbacks

      win <- atomically $ takeTMVar (windowValue glThread)
      statsRef <- newIORef (RenderStats win (V2 w h) 0)
      GLFW.setWindowSizeCallback win (Just $ resizeWindow statsRef)
      return $ OpenGLRendererState glThread mtLoader statsRef

    endGL (OpenGLRendererState glThread mtLoader _) = do
      shutdownAsyncLoader mtLoader
      -- terminateWindow is already called when the boundGLThread exits, so don't call it here
      endBoundGLThread glThread


-- | Checks for end events. Specifically any close events from GLFW or hitting the escapse key.
shouldEndProgram :: GLFW.Window -> IO Bool
shouldEndProgram win = do
  p <- GLFW.getKey win GLFW.Key'Escape
  GLFW.pollEvents
  windowKill <- GLFW.windowShouldClose win
  let shouldTerminate = p == GLFW.KeyState'Pressed ||  windowKill
  return shouldTerminate

-- | Given a scene graph, draw a single frame on an OpenGL canvas.
renderOneFrame :: SceneGraph OpenGLCommand dr -> Rec dr -> V2 Int -> IO ()
renderOneFrame scene frameData (V2 w h) = do
  GL.clearColor $= Color4 0 0.5 0.5 1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.depthFunc $= Just Less
  GL.cullFace $= Just Back
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  renderresult <- try $ openGLgo scene frameData
  case renderresult of
    Left e   -> putStrLn $ displayException (e :: SomeException)
    Right () -> return ()

openGLRendererFunctions :: Map Text BasicResource -> PlatformRendererFunctions OpenGLRendererState
openGLRendererFunctions prebuilt =
  PlatformRendererFunctions {
    wrapRenderLoop = openGLWrapRenderLoop prebuilt,
    getRendererStats = readIORef . openGLStats,
    runRenderFrame = \p scene frameData -> do
      stats <- readIORef (openGLStats p)
      writeIORef (openGLStats p) $ stats { frameCount = (frameCount stats) + 1}

      let boundGL = rendererBoundThread p
      let loaderInfo = rendererLoader p
      let sceneTarget = neutralToGLTarget scene

      -- generate new resource list and send to the loader
      let (GLDataSources sceneResources) = oglResourcesScene sceneTarget
      newRequest loaderInfo sceneResources
      -- grab whatever stuff has been loaded. Probably not all of the stuff we sent to the loader
      -- will be loaded yet, just grab what we can
      resources' <- getLoadedResources loaderInfo
      let rawResources = OpenGLResources resources'

      -- combine resources and OpenGLTarget scenegraph into an OpenGLCommand scenegraph and render that
      let sceneCommand = oglToCommand rawResources sceneTarget
      -- force the sceneCaommand and frameData thunks before passing them to the OGL thread
      sceneCommand `seq` frameData `seq`
        waitForPriorityGLTask boundGL $ do
          renderOneFrame sceneCommand frameData (windowSize stats)
          GLFW.swapBuffers (renderWindow stats)
      
      -- check for app end, this uses OpenGL so it needs to run in the OpenGL thread
      submitGLComputationThrow boundGL $ shouldEndProgram (renderWindow stats)
  }

