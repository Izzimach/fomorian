{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs, TypeSynonymInstances, TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE OverloadedLabels, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Sample where

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

import qualified Windowing as W
import SceneNode
import SceneResources

sampleRP :: FieldRec '[ '("a", GLint), '("windowSize", (GLint, GLint)) ]
sampleRP = undefined


buildPixelOrthoMatrix :: (Integral a) => a -> a -> M44 GLfloat
buildPixelOrthoMatrix w h =
  let 
    w1 = 1.0 / (fromIntegral w)
    h1 = 1.0 / (fromIntegral h)
    scaleMatrix x y z = V4 (V4 x 0 0 0)
                           (V4 0 y 0 0)
                           (V4 0 0 z 0)
                           (V4 0 0 0 1)
    translationMatrix x y z = V4 (V4 1 0 0 x)
                                 (V4 0 1 0 y)
                                 (V4 0 0 1 z)
                                 (V4 0 0 0 1)
    m1 = translationMatrix (-1) (-1) 0
    m2 = scaleMatrix (w1*2) (h1*2) 1
  in
    m1 !*! m2

data PixelOrthoNode n = PixelOrtho n
type PixelOrthoFrameFields = '[ '("windowX", Integer), '("windowY",Integer) ]
type PixelOrthoFrameData = FieldRec PixelOrthoFrameFields

orthize :: (fr ~ FieldRec ff, PixelOrthoFrameFields <: ff) => fr -> M44 GLfloat
orthize d = let rf = (rcast d) :: PixelOrthoFrameData
                w = rvalf #windowX rf
                h = rvalf #windowY rf
            in
              buildPixelOrthoMatrix w h

type OrthoParams = FieldRec '[ '("transformMatrix", M44 GLfloat) ]

instance (Drawable n (FieldRec ff)) => Drawable (PixelOrthoNode n) (FieldRec ff) where
  type FrameConstraints (PixelOrthoNode n) (FieldRec ff) = 
    (PixelOrthoFrameFields <: ff, Drawable n OrthoParams, FrameConstraints n OrthoParams )
  draw (PixelOrtho n) (PerFrameData fr) res =
      let orthoM = orthize fr
          frX = PerFrameData $ (#transformMatrix =: orthoM) :& RNil
      in
        draw n frX res

instance (NeedsResources n) => NeedsResources (PixelOrthoNode n) where
  resources (PixelOrtho n) = resources n

sampleScene = let snode = (  
                         (#shader =: "linez")
                      :& (#shaderParameters =: ((#tex =: (0 :: GLint)) :& RNil) )
                      :& (#vertexBuffers =: [
                            V2Data [V2 10 10, V2 100 10, V2 10 100, V2 100 100],
                            IndexData [0,1,2, 2,1,3]
                          ]
                         )
                      :& (#textures =: ["lollipopGreen.png"])
                      :& RNil
                      ) -- :: (SceneNodeR (FieldRec '[ '("tex", GLint)]))
             in SG (PixelOrtho (Invoke snode)) undefined
   
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

