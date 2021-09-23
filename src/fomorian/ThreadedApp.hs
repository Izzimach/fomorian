{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Fomorian.ThreadedApp where

import Control.Monad

import Data.IORef
import Data.Row
import Data.Row.Records

import Linear


import Fomorian.SceneNode
import Fomorian.NeutralSceneTarget

import Fomorian.SimpleApp

type family RendererResources renderState

newtype AppState rw = AppState (Rec rw)


-- | A Specific API target such as OpenGL or Vulkan provides it's own specific set of PlatFormRendererFunctions
data PlatformRendererFunctions renderState j =
  PlatformRendererFunctions {
    wrapRenderLoop :: (Int,Int) -> (renderState -> IO ()) -> IO (),
    getAppInfo :: renderState -> IORef (AppState j),
    runRenderFrame :: renderState -> SceneGraph NeutralSceneTarget TopLevel3DRow -> Rec TopLevel3DRow -> IO Bool
  }

-- | Given app state generates some default frame parameters
threadedAppRenderParams :: (HasType "curTime" Float rw, HasType "windowSize" (Int,Int) rw) => AppState rw -> Rec TopLevel3DRow
threadedAppRenderParams (AppState appstate) =
  let t     = appstate .! #curTime
      (w,h) = appstate .! #windowSize
  in   (#modelMatrix .== (identity :: M44 Float)) .+
       (#viewMatrix .== (identity :: M44 Float)) .+
       (#projectionMatrix .== (identity :: M44 Float)) .+
       (#curTime .== t) .+
       (#windowX .== fromIntegral w) .+
       (#windowY .== fromIntegral h)


-- | A basic app that just runs a render function over and over.
threadedApp :: (HasType "curTime" Float j, HasType "windowSize" (Int,Int) j) => 
  (Int, Int) ->
  PlatformRendererFunctions r j ->
  (AppState j -> SceneGraph NeutralSceneTarget TopLevel3DRow) ->
  IO ()
threadedApp (w,h) p sceneFunc = (wrapRenderLoop p (w,h) threadedGo)
  where
    threadedGo rendererState = do
      let appdata = getAppInfo p rendererState
      renderLoopThreaded appdata sceneFunc threadedAppRenderParams p rendererState

-- | Runs a render loop by generating a scene graph and frame parameters from app state.
renderLoopThreaded :: (HasType "curTime" Float j) =>
  -- | IORef to app data
  IORef (AppState j)->
  -- | function to generate a scene graph from the app data
  (AppState j -> SceneGraph NeutralSceneTarget TopLevel3DRow) ->
  -- | Produce frame data to pass to the render function given some app data
  (AppState j -> Rec TopLevel3DRow) ->
  -- | Renderer functions, provided by OpenGL or Vulkan modules
  PlatformRendererFunctions r j ->
  -- | Renderer State, produced by the renderer
  r ->
  -- | Runs as a loop
  IO ()
renderLoopThreaded appref buildScene genFD p rST = loop
  where
    loop = do
      -- convert app state into a scene graph
      (AppState appstate) <- readIORef appref
      let curTime' = (appstate .! #curTime) + 0.016
      let appstate' = AppState (update #curTime curTime' appstate)
      writeIORef appref appstate'

      let neutralScene = buildScene appstate'
      let frameData = genFD appstate'
      shouldTerminate <- runRenderFrame p rST neutralScene frameData

      -- swap in the just rendered scene and check for termination
      unless shouldTerminate loop


