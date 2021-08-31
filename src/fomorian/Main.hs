module Fomorian.Main where

import qualified Fomorian.Sample (main, testScene3d)
--import qualified Fomorian.Vulkan.Example (main)

import Fomorian.ThreadedApp
import Fomorian.OpenGL.PlatformRenderer

main :: IO ()
main = do
  --Fomorian.Sample.main
  threadTest


threadTest :: IO ()
threadTest = threadedApp (600,400) openGLRendererFunctions (const Fomorian.Sample.testScene3d)

