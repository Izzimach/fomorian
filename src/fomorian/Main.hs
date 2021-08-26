module Fomorian.Main where

import qualified Fomorian.Sample (main)
import qualified Fomorian.Vulkan.Example (main)

main :: IO ()
main = do
  Fomorian.Sample.main

