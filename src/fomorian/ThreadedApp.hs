{-# LANGUAGE OverloadedLabels #-}

module Fomorian.ThreadedApp where

import Control.Monad

import Data.IORef
import Data.Row
import Data.Row.Records



import Fomorian.SceneNode
import Fomorian.NeutralSceneTarget

import Fomorian.SimpleApp


data PlatformRendererFunctions x =
  PlatformRendererFunctions {
    initializeRenderer :: (Int,Int) -> IO x,
    getAppInfo :: x -> IORef AppInfo,
    runRenderFrame :: x -> SceneGraph NeutralSceneTarget TopLevel3DRow -> Rec TopLevel3DRow -> IO Bool,
    shutdownRenderer :: x -> IO ()
  }

-- | A basic app that just runs a render function over and over.
threadedApp :: (Int, Int) -> PlatformRendererFunctions x -> (AppInfo -> SceneGraph NeutralSceneTarget TopLevel3DRow) -> IO ()
threadedApp (w,h) p renderFunc = do
  rendererState <- (initializeRenderer p (w,h))
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
  PlatformRendererFunctions x ->
  x -> 
  IO ()
renderLoopThreaded appref buildScene genFD p rST = loop
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


