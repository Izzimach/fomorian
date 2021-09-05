module Fomorian.Main where

import qualified Fomorian.Sample (main, testScene3d)
--import qualified Fomorian.Vulkan.Example (main)

import Fomorian.ThreadedApp
import Fomorian.OpenGL.PlatformRenderer
--import Fomorian.Vulkan.PlatformRenderer

main :: IO ()
main = do
  --Fomorian.Sample.main
  threadTest


threadTest :: IO ()
threadTest =
  let renderer = openGLRendererFunctions {-vulkanRendererFunctions-}
  in threadedApp (600,400) renderer (const Fomorian.Sample.testScene3d)

