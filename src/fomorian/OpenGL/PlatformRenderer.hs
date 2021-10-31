{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Fomorian.OpenGL.PlatformRenderer where

import Control.Exception
import Control.Concurrent.STM

import Data.IORef
import Data.Row

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

data OpenGLRendererState =
  OpenGLRendererState {
    rendererBoundThread :: BoundGLThread GLFW.Window,
    rendererLoader :: AsyncLoader (DataSource GLDataSourceTypes) (Resource GLResourceTypes) ErrorMessage,
    openGLStats :: IORef RenderStats
  }

-- | Given a bound OpenGL thread 'BoundGLThread' will generate a 'ResourceLoaderConfig' that offloads
--   loading and unloading to the OpenGL thread. We try to force the values using 'seq' to make thunk evaluation
--   take place in the worker thread and not the OpenGL thread. There is only one OpenGL thread so we don't want to
--   load it down with extra work evaluating thunks.
threadedLoaderGLConfig :: BoundGLThread w -> LoadUnloadCallbacks (DataSource GLDataSourceTypes) (Resource GLResourceTypes) ErrorMessage
threadedLoaderGLConfig glb =
  LoadUnloadCallbacks {
    loadResource     = \l deps -> l `seq` deps `seq` submitGLComputationThrow glb (loadGLResource l deps),
    unloadResource   = \res -> submitGLComputationThrow glb (unloadGLResource res),
    findDependencies = computeGLDependencies,
    processException = const Drop
    }

openGLWrapRenderLoop :: (Int, Int) -> (OpenGLRendererState -> IO ()) -> IO ()
openGLWrapRenderLoop (w,h) wrapped = bracket startGL endGL wrapped
  where
    startGL = do
      let initData = WindowInitData w h "OpenGL Fomorian" UseOpenGL
      glThread <- forkBoundGLThread (W.initWindow initData) terminateWindow
      -- spin up concurrent loader
      let asyncConfig = AsyncLoaderConfig (threadedLoaderGLConfig glThread) 1 simpleThreadWrapper simpleThreadWrapper
      loaderInfo <- startAsyncLoader asyncConfig ()
      win <- atomically $ takeTMVar (windowValue glThread)
      --appdata <- submitGLComputationThrow glThread $ initAppState initData win
      statsRef <- newIORef (RenderStats win (V2 w h) 0)
      return $ OpenGLRendererState glThread loaderInfo statsRef

    endGL (OpenGLRendererState glThread loaderInfo _) = do
      shutdownAsyncLoader loaderInfo
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

openGLRendererFunctions :: PlatformRendererFunctions OpenGLRendererState
openGLRendererFunctions =
  PlatformRendererFunctions {
    wrapRenderLoop = openGLWrapRenderLoop,
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
          renderOneFrame sceneCommand frameData
          GLFW.swapBuffers (renderWindow stats)
      
      -- check for app end, this uses OpenGL so it needs to run in the OpenGL thread
      submitGLComputationThrow boundGL $ shouldEndProgram (renderWindow stats)
  }

