{-# LANGUAGE OverloadedLabels #-}

module Main where

import Test.Hspec
import qualified Fomorian.Windowing as W
import qualified Fomorian.SceneNode as Scene
import Fomorian.Common (TopWindowFrameParams)
import Control.Exception

import Data.Vinyl


--
-- basic test code to open a window, run/render it for one second, then close
--
genRenderParams :: W.AppInfo -> TopWindowFrameParams
genRenderParams appstate =
  let (w,h) = rvalf #windowSize appstate
      t     = rvalf #curTime appstate
  in   (#windowX =: fromIntegral w)
    :& (#windowY =: fromIntegral h)
    :& (#curTime =: t)
    :& RNil




testRun :: IO ()
testRun = do
  let scene = Scene.group []
  let windowConfig = (600,400,"Demo")
  let initfunc = W.initWindow windowConfig >>= return
  let endfunc  = \win -> W.terminateWindow win
  let runForOneSecond = \appinfo -> let t = rgetf #curTime appinfo
                                    in
                                      if t < 1.0 then W.NextFrame else W.EndApp
  let loopfunc = \win -> do
                           appdata <- W.initAppState windowConfig win
                           W.renderLoop appdata (const scene) genRenderParams runForOneSecond
  bracket initfunc endfunc loopfunc



main :: IO ()
main = hspec $ do
  describe "Windowing" $ do
    it "can open and close a window without generating an exception" $ do
      testRun `shouldReturn` ()