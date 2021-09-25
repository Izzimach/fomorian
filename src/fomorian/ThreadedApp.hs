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

import Fomorian.PlatformRenderer
import Fomorian.SceneNode
import Fomorian.NeutralSceneTarget


-- | A basic app that just runs a render function over and over.
threadedApp :: (Int, Int) ->
  PlatformRendererFunctions r ->
  s ->
  (s -> (s, Bool)) ->
  (s -> SceneGraph NeutralSceneTarget DefaultDrawFrameParams) ->
  IO ()
threadedApp (w,h) p initialState updateFunc sceneFunc = (wrapRenderLoop p (w,h) threadedGo)
  where
    threadedGo rendererState = do
      renderLoopThreaded initialState updateFunc sceneFunc p rendererState

-- | Runs a render loop by generating a scene graph and frame parameters from app state.
renderLoopThreaded :: 
  -- | Initial app state
  s ->
  -- | Update state function, called before each frame
  (s -> (s, Bool)) -> 
  -- | function to generate a scene graph from the app data
  (s -> SceneGraph NeutralSceneTarget DefaultDrawFrameParams) ->
  -- | Produce frame data to pass to the render function given some app data
--  (s-> Rec DefaultDrawFrameParams) ->
  -- | Renderer functions, provided by OpenGL or Vulkan modules
  PlatformRendererFunctions r ->
  -- | Renderer State, produced by the renderer
  r ->
  -- | Runs as a loop
  IO ()
renderLoopThreaded initialState updateState buildScene {-genFD-} p rST = loop initialState
  where
    loop appState = do
      stats <- getRendererStats p rST
      let (appState', appQuit) = updateState appState

      let neutralScene = buildScene appState'
      let frameData = generateDefaultDrawFrameParameters stats -- genFD appState'
      shouldTerminate <- runRenderFrame p rST neutralScene frameData

      -- swap in the just rendered scene and check for termination
      unless (shouldTerminate || appQuit) (loop appState')


