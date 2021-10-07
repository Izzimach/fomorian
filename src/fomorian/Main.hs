{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Fomorian.Main where

import qualified Fomorian.Sample (testScene3d)

import Fomorian.ThreadedApp
import Fomorian.OpenGL.PlatformRenderer
--import Fomorian.Vulkan.PlatformRenderer

main :: IO ()
main = do
  --Fomorian.Sample.main
  threadTest

data AppState = AppState Int

oneHundredFrames :: AppState -> (AppState, Bool)
oneHundredFrames (AppState s) = (AppState (s+1), s > 100)

threadTest :: IO ()
threadTest =
  let renderer = openGLRendererFunctions
  --let renderer = vulkanRendererFunctions
      initialState = AppState 0
  in
      threadedApp (600,400) renderer initialState oneHundredFrames (const Fomorian.Sample.testScene3d)

