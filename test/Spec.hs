{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec where


import Data.Maybe (fromJust)
import Data.Text.Lazy (pack)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Map.Strict as M

import MemoryArenaTests


mainTest :: IO Bool
mainTest = do
  test1 <- memoryArenaTests
  return test1

