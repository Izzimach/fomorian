{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs, TypeSynonymInstances, TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE OverloadedLabels, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module SampleFinal where


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
import Control.Monad.Trans
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import System.FilePath ((</>))

import qualified Windowing as W

import SceneFinal
import SceneResources


squareRPProxy :: RenderParamsProxy (FieldRec '[
                    '("cameraProjection", M44 GLfloat),
                    '("worldTransform", M44 GLfloat)
                  ])
squareRPProxy = RPProxy

simpleSquare file = invoke $ Invocation $
          (#shader =: "linez")
      :&  (#shaderParameters =: ((#tex =: (0 :: GLint)) :& RNil) )
      :&  (#vertexBuffers =: [
              V2Data [V2 10 10, V2 100 10, V2 10 100, V2 100 100],
              IndexData [0,1,2, 2,1,3]
            ]
          )
      :&  (#textures =: [file])
      :&  (#rpProxy =: squareRPProxy)
      :&  RNil



testScene :: (SceneSYM repr) => repr (FieldRec PixelOrthoFrameFields)
testScene = ortho2DView $
              simpleSquare "lollipopGreen.png"
         
t2 :: (MonadIO m) => DrawGL m (FieldRec PixelOrthoFrameFields)
t2 = testScene
              
genRenderParams :: W.AppInfo -> FieldRec PixelOrthoFrameFields
genRenderParams appstate = let (w,h) = rvalf #windowSize appstate
  in   (#windowX =: fromIntegral w)
    :& (#windowY =: fromIntegral h)
    :& RNil


main :: IO ()
main = do
  let drawM = runDrawGL testScene
  let renderParams = (#windowX =: 100) :& (#windowY =: 100) :& RNil
  drawM renderParams

