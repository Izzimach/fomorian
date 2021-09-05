{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Fomorian.OpenGL.PlatformRenderer where

import Control.Monad
import Control.Exception
import Control.Concurrent.STM

import Data.IORef

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import LoadUnload
import AsyncLoader

import Fomorian.Windowing as W
import Fomorian.SceneNode
import Fomorian.SceneResources

import Fomorian.OpenGL.GLBoundThread
import Fomorian.OpenGL.OpenGLResources
import Fomorian.OpenGL.OpenGLCommand

import Fomorian.ThreadedApp
import Fomorian.SimpleApp

-- Code to fire up the loader and bound thread, and then route scene processing through the various parts.
--
-- For a diagram of threads and data flow look at OpenGLFlow.svg

type instance RendererResources OpenGLRendererState = LoadedResources (DataSource GLDataSourceTypes) (Resource GLResourceTypes)


data OpenGLRendererState =
  OpenGLRendererState {
    rendererBoundThread :: BoundGLThread GLFW.Window,
    rendererLoader :: ForkLoaderResult (LoaderRequest (DataSource GLDataSourceTypes) (Resource GLResourceTypes)) (LoaderResult (DataSource GLDataSourceTypes) (Resource GLResourceTypes)),
    rendererAppInfo :: IORef (AppInfo (RendererResources OpenGLRendererState)),
    rendererWindow :: GLFW.Window
  }

-- | Given a bound OpenGL thread 'BoundGLThread' will generate a 'ResourceLoaderConfig' that offloads
--   loading and unloading to the OpenGL thread. We try to force the values using 'seq' to make thunk evaluation
--   take place in the worker thread and not the OpenGL thread. There is only one OpenGL thread so we don't want to
--   load it down with extra work evaluating thunks.
threadedLoaderGLConfig :: BoundGLThread w -> ResourceLoaderConfig (DataSource GLDataSourceTypes) (Resource GLResourceTypes)
threadedLoaderGLConfig glb = ResourceLoaderConfig {
  loadIO = (\l deps -> l `seq` deps `seq` submitGLComputationThrow glb (loadGLResource l deps)),
  unloadIO = (\l r -> l `seq` r `seq` submitGLComputationThrow glb (unloadGLResource l r)),
  dependenciesIO = computeGLDependencies
  }

openGLWrapRenderLoop :: (Int, Int) -> (OpenGLRendererState -> IO ()) -> IO ()
openGLWrapRenderLoop (w,h) wrapped = bracket startGL endGL wrapped
  where
    startGL = do
      let initData = WindowInitData w h "Haskell App" UseOpenGL
      glThread <- forkBoundGLThread (W.initWindow initData) (\win -> terminateWindow win)
      -- spin up concurrent loader
      loaderInfo <- forkLoader 4 (threadedLoaderGLConfig glThread)
      win <- atomically $ takeTMVar (windowValue glThread)
      appdata <- submitGLComputationThrow glThread $ initAppState initData win
      return $ OpenGLRendererState glThread loaderInfo appdata win

    endGL (OpenGLRendererState glThread loaderInfo _ win) = do
      shutdownLoader loaderInfo
      -- terminateWindow is already called when the boundGLThread exits, so don't call it here
      endBoundGLThread glThread

openGLRendererFunctions :: PlatformRendererFunctions OpenGLRendererState
openGLRendererFunctions =
  PlatformRendererFunctions {
    wrapRenderLoop = openGLWrapRenderLoop,
    getAppInfo = rendererAppInfo,
    runRenderFrame = \p scene frameData -> do
      let boundGL = rendererBoundThread p
      let loaderInfo = rendererLoader p

      let sceneTarget = neutralToGLTarget scene

      -- generate new resource list and send to the loader
      let (GLDataSources sceneResources) = oglResourcesScene sceneTarget
      atomically $ writeTVar (stmRequest loaderInfo) (LoaderRequest sceneResources)
      -- grab whatever stuff has been loaded. Probably not all of the stuff we sent to the loader
      -- will be loaded yet, just grab what we can
      (LoaderResult resources') <- atomically $ readTVar (stmResult loaderInfo)
      let rawResources = OpenGLResources resources'

      -- combine resources and OpenGLTarget scenegraph into an OpenGLCommand scenegraph and render that
      let sceneCommand = oglToCommand rawResources sceneTarget
      -- force the sceneCaommand and frameData thunks before passing them to the OGL thread
      sceneCommand `seq` frameData `seq`
        waitForPriorityGLTask boundGL $ do
          renderOneFrame sceneCommand frameData
          GLFW.swapBuffers (rendererWindow p)
      
      -- check for app end, this uses OpenGL so it needs to run in the OpenGL thread
      submitGLComputationThrow boundGL $ shouldEndProgram (rendererWindow p)
  }

