{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs, TypeSynonymInstances, TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE OverloadedLabels, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Fomorian.Sample where

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU
import Linear
import Data.Vinyl
import Data.Word (Word32)
import Graphics.VinylGL

import Data.IORef
import Data.Maybe (mapMaybe)
import Data.Bifunctor (bimap)
import Control.Monad
import Control.Lens ((^.), (.~))
import Control.Exception
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import System.FilePath ((</>))

import qualified Fomorian.Windowing as W

import Fomorian.SceneNode
import Fomorian.SceneResources


sampleScene = let texturedSquare file = Invoke (  
                         (#shader =: "linez")
                      :& (#shaderParameters =: ((#tex =: (0 :: GLint)) :& RNil) )
                      :& (#vertexBuffers =: [
                            V2Data [V2 10 10, V2 100 10, V2 10 100, V2 100 100],
                            IndexData [0,1,2, 2,1,3]
                          ]
                         )
                      :& (#textures =: [file])
                      :& RNil
                      ) -- :: (SceneNodeR (FieldRec '[ '("tex", GLint)]))
                  scene = PixelOrthoView $
                            translateNode (V3 100 0 0) $
                              texturedSquare "lollipopGreen.png"
             in SG scene undefined
   
genRenderParams :: W.AppInfo -> PerFrameData ('[ '("a",GLint), '("windowX",Integer), '("windowY",Integer) ] )
genRenderParams appstate = let (w,h) = rvalf #windowSize appstate
  in
    PerFrameData $
             (#a =: (23 :: GLint) )
          :& (#windowX =: fromIntegral w)
          :& (#windowY =: fromIntegral h)
--                     (#transformMatrix =: buildPixelOrthoMatrix (rvalf #windowSize appstate))
          :& RNil


main :: IO ()
main = do
  let scene = sampleScene
  let windowConfig = (400,400,"Demo")
  win <- W.initWindow windowConfig
  appdata <- W.initAppState windowConfig win
  W.renderLoop appdata (const scene) genRenderParams
  W.terminateWindow win

