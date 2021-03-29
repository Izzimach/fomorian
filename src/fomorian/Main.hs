module Main where

import qualified Fomorian.Sample (main)
import qualified Fomorian.Vulkan.Vulkan (main)

main :: IO ()
main = do
  Fomorian.Vulkan.Vulkan.main

  

