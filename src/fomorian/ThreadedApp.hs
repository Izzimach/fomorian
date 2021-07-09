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
import Fomorian.SceneResources
import Fomorian.OpenGLResources
import Fomorian.Windowing
import Fomorian.OpenGLCommand
import Fomorian.SimpleApp
import Fomorian.OpenGLResources
import Fomorian.GLBoundThread

import Fomorian.Sample

threadedLoaderGLConfig :: BoundGLThread w -> ResourceLoaderConfig (DataSource GLDataSourceTypes) (Resource GLResourceTypes)
threadedLoaderGLConfig glb = ResourceLoaderConfig {
  loadIO = (\l deps -> submitGLComputationThrow glb (loadGLResource l deps)),
  unloadIO = (\l r -> submitGLComputationThrow glb (unloadGLResource l r)),
  dependenciesIO = computeGLDependencies
  }



-- | A basic app that just runs a render function over and over.
threadedApp :: (Int, Int) -> (AppInfo -> SceneGraph TopLevel3DRow OpenGLTarget) -> IO ()
threadedApp (w,h) renderFunc = do
  let initData = WindowInitData w h "Haskell App" UseOpenGL
  let initfunc = initWindow initData
  let endfunc  = \win -> terminateWindow win
  glThread <- forkBoundGLThread initfunc endfunc
  loaderInfo <- forkLoader 4 (threadedLoaderGLConfig glThread)
  win <- atomically $ takeTMVar (windowValue glThread)
  let loopfunc = \win -> do
                           appdata <- initAppState initData win
                           renderLoopThreaded appdata renderFunc simpleAppRenderParams glThread loaderInfo
  loopfunc win
  shutdownLoader loaderInfo
  endBoundGLThread glThread



-- | Runs a render loop by generating a scene graph and frame parameters from app state.
renderLoopThreaded :: IORef AppInfo -> (AppInfo -> SceneGraph r OpenGLTarget ) -> (AppInfo -> Rec r) -> BoundGLThread w ->
  -- yikes, use some type synonyms to fix this
  ForkLoaderResult (LoaderRequest (DataSource GLDataSourceTypes) (Resource GLResourceTypes)) (LoaderResult (DataSource GLDataSourceTypes) (Resource GLResourceTypes))-> IO ()
renderLoopThreaded appref buildScene genFD boundGL loaderInfo = loop
  where
    loop = do
      appstate <- readIORef appref
      let win = appstate .! #window
      let sceneTarget = buildScene appstate

      -- generate new resource list and send to the loader
      let (GLDataSources sceneResources) = oglResourcesScene sceneTarget
      atomically $ writeTVar (stmRequest loaderInfo) (LoaderRequest sceneResources)

      let curTime' = (appstate .! #curTime) + 0.016
      let appstate' = update #curTime curTime' $ {-update #resources resources' $ -} appstate

      writeIORef appref appstate'

      -- grab whatever stuff has been loaded. Probably not all of the stuff we sent ot the loader
      -- will be loaded!
      (LoaderResult resources') <- atomically $ readTVar (stmResult loaderInfo)
      let rawResources = OpenGLResources resources'

      -- generate the draw commands
      let sceneCommand = oglToCommand rawResources sceneTarget
      let frameData = genFD appstate'
      waitForPriorityGLTask boundGL $ renderOneFrame sceneCommand frameData

      GLFW.swapBuffers win
      shouldTerminate <- shouldEndProgram appstate'
      unless shouldTerminate loop

threadTest :: IO ()
threadTest = simpleApp (600,400) (const testScene3d)
