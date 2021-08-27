{-# LANGUAGE OverloadedLabels #-}

module Fomorian.ThreadedApp where

import Control.Monad
import Control.Exception
import Control.Concurrent.STM

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import qualified Data.Map as M
import Data.IORef
import Data.Row
import Data.Row.Records

import LoadUnload
import AsyncLoader


import Fomorian.SceneNode
import Fomorian.NeutralSceneTarget
import Fomorian.SceneResources
import Fomorian.OpenGLResources
import Fomorian.Windowing
import Fomorian.OpenGLCommand
import Fomorian.SimpleApp
import Fomorian.OpenGLResources
import Fomorian.GLBoundThread

import Fomorian.Sample


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

data PlatformRendererFunctions x =
  PlatformRendererFunctions {
    initializeRenderer :: (Int,Int) -> IO x,
    getAppInfo :: x -> IORef AppInfo,
    runRenderFrame :: x -> SceneGraph NeutralSceneTarget TopLevel3DRow -> Rec TopLevel3DRow -> IO Bool,
    shutdownRenderer :: x -> IO ()
  }

data OpenGLRendererState =
  OpenGLRendererState {
    rendererBoundThread :: BoundGLThread GLFW.Window,
    rendererLoader :: ForkLoaderResult (LoaderRequest (DataSource GLDataSourceTypes) (Resource GLResourceTypes)) (LoaderResult (DataSource GLDataSourceTypes) (Resource GLResourceTypes)),
    rendererAppInfo :: IORef AppInfo,
    rendererWindow :: GLFW.Window
  }

openGLRendererFunctions :: PlatformRendererFunctions OpenGLRendererState
openGLRendererFunctions =
  PlatformRendererFunctions {

    initializeRenderer = \(w,h) -> do
      let initData = WindowInitData w h "Haskell App" UseOpenGL
      glThread <- forkBoundGLThread (initWindow initData) (\win -> terminateWindow win)
      -- spin up concurrent loader
      loaderInfo <- forkLoader 4 (threadedLoaderGLConfig glThread)
      win <- atomically $ takeTMVar (windowValue glThread)
      appdata <- submitGLComputationThrow glThread $ initAppState initData win
      return $ OpenGLRendererState glThread loaderInfo appdata win
    ,
    getAppInfo = rendererAppInfo
    ,
    runRenderFrame = \p scene frameData -> do
      let sceneTarget = neutralToGLTarget scene
      let boundGL = rendererBoundThread p
      let loaderInfo = rendererLoader p

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
    ,
    shutdownRenderer = \p -> do
      shutdownLoader (rendererLoader p)
      endBoundGLThread (rendererBoundThread p)

  }

-- | A basic app that just runs a render function over and over.
threadedApp :: (Int, Int) -> PlatformRendererFunctions OpenGLRendererState -> (AppInfo -> SceneGraph NeutralSceneTarget TopLevel3DRow) -> IO ()
threadedApp (w,h) p renderFunc = do
  -- fork the OpenGL thread and the loader thread(s)
  rendererState <- (initializeRenderer p (w,h))
  -- get window from the opengl thread and start up the render loop
  let appdata = getAppInfo p rendererState
  renderLoopThreaded appdata renderFunc simpleAppRenderParams p rendererState
  -- done, show down stuff
  shutdownRenderer p rendererState

-- | Runs a render loop by generating a scene graph and frame parameters from app state.
renderLoopThreaded ::
  -- | IORef to app data
  IORef AppInfo ->
  -- | function to generate a scene graph from the app data
  (AppInfo -> SceneGraph NeutralSceneTarget TopLevel3DRow) ->
  -- | Produce frame data to pass to the render function given some app data
  (AppInfo -> Rec TopLevel3DRow) ->
  PlatformRendererFunctions OpenGLRendererState ->
  OpenGLRendererState -> 
  -- | The bound OpenGL thread generated in 'forkBoundGLThread'
  --BoundGLThread w ->
  -- | this mess was passed back from 'forkLoader'
  --ForkLoaderResult (LoaderRequest (DataSource GLDataSourceTypes) (Resource GLResourceTypes)) (LoaderResult (DataSource GLDataSourceTypes) (Resource GLResourceTypes)) ->
  IO ()
renderLoopThreaded appref buildScene genFD p rST {-boundGL loaderInfo-} = loop
  where
    loop = do
      -- convert app state into a scene graph
      appstate <- readIORef appref
      let curTime' = (appstate .! #curTime) + 0.016
      let appstate' = update #curTime curTime' appstate
      writeIORef appref appstate'

      let neutralScene = buildScene appstate'
      let frameData = genFD appstate'
      shouldTerminate <- runRenderFrame p rST neutralScene frameData

      -- swap in the just rendered scene and check for termination
      unless shouldTerminate loop


threadTest :: IO ()
threadTest = threadedApp (600,400) openGLRendererFunctions (const testScene3d)

