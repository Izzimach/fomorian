{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Fomorian.PlatformRenderer where

import Data.Row

import Linear

import qualified Graphics.UI.GLFW as GLFW

import Fomorian.SceneNode
import Fomorian.NeutralSceneTarget

data RenderStats = 
  RenderStats
  {
    -- | handle to the window provided by GLFW
    renderWindow:: GLFW.Window,
    -- | size in pixels
    windowSize :: V2 Int,
    -- | how many frames have been rendered
    frameCount :: Int
  }
  deriving (Eq, Show)


-- | A Specific API target such as OpenGL or Vulkan provides it's own renderState and specific set of PlatFormRendererFunctions.
data PlatformRendererFunctions renderState =
  PlatformRendererFunctions {
    wrapRenderLoop :: (Int,Int) -> (renderState -> IO ()) -> IO (),
    getRendererStats :: renderState -> IO RenderStats,
    runRenderFrame :: renderState -> SceneGraph NeutralSceneTarget DefaultDrawFrameParams -> Rec DefaultDrawFrameParams -> IO Bool
  }

-- | This is all the scene graph gets by default. Usually a 'Transformer' will add things like the projection matrix and model/view matrix
type DefaultDrawFrameParams =
  "windowX"    .== Integer .+
  "windowY"    .== Integer .+ 
  "curTime"    .== Float   .+
  "correctNDC" .== M44 Float

-- | Some default per-frame draw parameters generated from 'RenderStats'
generateDefaultDrawFrameParameters :: RenderStats -> Rec DefaultDrawFrameParams
generateDefaultDrawFrameParameters stats =
  let (V2 w h) = windowSize stats
      t        = (fromIntegral $ frameCount stats) * (0.016 :: Float) -- assume 60 fps
  in  (#curTime .== t) .+
      (#windowX .== fromIntegral w) .+
      (#windowY .== fromIntegral h) .+
      (#correctNDC .== Linear.identity)
