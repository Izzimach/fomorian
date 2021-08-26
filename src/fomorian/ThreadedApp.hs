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
--   loading and unloading to the OpenGL thread.
threadedLoaderGLConfig :: BoundGLThread w -> ResourceLoaderConfig (DataSource GLDataSourceTypes) (Resource GLResourceTypes)
threadedLoaderGLConfig glb = ResourceLoaderConfig {
  loadIO = (\l deps -> submitGLComputationThrow glb (loadGLResource l deps)),
  unloadIO = (\l r -> submitGLComputationThrow glb (unloadGLResource l r)),
  dependenciesIO = computeGLDependencies
  }


-- | A basic app that just runs a render function over and over.
threadedApp :: (Int, Int) -> (AppInfo -> SceneGraph NeutralSceneTarget TopLevel3DRow) -> IO ()
threadedApp (w,h) renderFunc = do
  let initData = WindowInitData w h "Haskell App" UseOpenGL
  -- fork the OpenGL thread and the loader thread(s)
  glThread <- forkBoundGLThread (initWindow initData) (\win -> terminateWindow win)
  -- spin up concurrent loader
  loaderInfo <- forkLoader 4 (threadedLoaderGLConfig glThread)
  -- get window from the opengl thrad and start up the render loop
  win <- atomically $ takeTMVar (windowValue glThread)
  appdata <- submitGLComputationThrow glThread $ initAppState initData win
  renderLoopThreaded appdata renderFunc simpleAppRenderParams glThread loaderInfo
  -- done, show down stuff
  shutdownLoader loaderInfo
  endBoundGLThread glThread

-- | Runs a render loop by generating a scene graph and frame parameters from app state.
renderLoopThreaded ::
  -- | IORef to app data
  IORef AppInfo ->
  -- | function to generate a scene graph from the app data
  (AppInfo -> SceneGraph NeutralSceneTarget dr) ->
  -- | Produce frame data to pass to the render function given some app data
  (AppInfo -> Rec dr) ->
  -- | The bound OpenGL thread generated in 'forkBoundGLThread'
  BoundGLThread w ->
  -- | this mess was passed back from 'forkLoader'
  ForkLoaderResult (LoaderRequest (DataSource GLDataSourceTypes) (Resource GLResourceTypes)) (LoaderResult (DataSource GLDataSourceTypes) (Resource GLResourceTypes)) ->
  IO ()
renderLoopThreaded appref buildScene genFD boundGL loaderInfo = loop
  where
    loop = do
      -- convert app state into a scene graph
      appstate <- readIORef appref
      let sceneTarget = neutralToGLTarget $ buildScene appstate

      -- generate new resource list and send to the loader
      let (GLDataSources sceneResources) = oglResourcesScene sceneTarget
      atomically $ writeTVar (stmRequest loaderInfo) (LoaderRequest sceneResources)

      -- update time in app state
      let curTime' = (appstate .! #curTime) + 0.016
      let appstate' = update #curTime curTime' appstate
      writeIORef appref appstate'

      -- grab whatever stuff has been loaded. Probably not all of the stuff we sent to the loader
      -- will be loaded yet, just grab what we can
      (LoaderResult resources') <- atomically $ readTVar (stmResult loaderInfo)
      let rawResources = OpenGLResources resources'

      -- combine resources and OpenGLTarget scenegraph into an OpenGLCommand scenegraph and render that
      let sceneCommand = oglToCommand rawResources sceneTarget
      let frameData = genFD appstate'
      waitForPriorityGLTask boundGL $ do
        renderOneFrame sceneCommand frameData
        GLFW.swapBuffers (appstate' .! #window)
      
      shouldTerminate <- submitGLComputationThrow boundGL $ shouldEndProgram appstate'

      -- swap in the just rendered scene and check for termination
      unless shouldTerminate loop


threadTest :: IO ()
threadTest = threadedApp (600,400) (const testScene3d)


boundGLGo :: BoundGLThread GLFW.Window -> IO ()
boundGLGo glt = do
  submitGLComputationThrow glt $ putStrLn "in GL thread"
       
