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


import Linear
import Data.Row
import Data.Word (Word32)
import qualified Data.Constraint as DC

import Data.IORef
import Data.Maybe (mapMaybe)
import Data.Bifunctor (bimap)
import Data.Functor.Foldable
import Control.Monad
import Control.Lens ((^.), (.~), (%~))
import Control.Exception
import Control.Monad.Trans
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import System.FilePath ((</>))

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU
import qualified Graphics.UI.GLFW as GLFW


--import qualified Fomorian.Windowing as W

import Fomorian.SceneNode
import Fomorian.Windowing
import Fomorian.SimpleApp
import Fomorian.SceneResources
import Fomorian.OpenGLCommand
import Fomorian.CommonSceneNodes

--
-- 2d draw test
--
{-
simpleSquare file = Fix $ Invoke $ 
          (#shader =: "linez")
      :&  (#staticParameters =: ((#tex =: (0 :: GLint)) :& RNil) )
      :&  (#frameParameters  =: (
                                    (#cameraProjection =: (identity :: M44 GLfloat))
                                 :& (#worldTransform =: (identity :: M44 GLfloat))
                                 :& (#curTime =: (0 :: GLfloat))
                                 :& RNil )
          )
      :&  (#vertexBuffers =: [
              V2Data [V2 0 0, V2 100 0, V2 0 100, V2 100 100],
              T2Data [V2 0 1, V2 1 1, V2 0 0, V2 1 0],
              IndexData [0,2,1, 2,3,1]
            ]
          )
      :&  (#textures =: [file])
      :&  RNil

testScene = pixelOrtho2DView $
              group
              [
                translate2d (V2 0 0)    $ simpleSquare "sad-crab.png",
                translate2d (V2 150 50) $ simpleSquare "owl.png"
              ]


--
-- 3d draw test
--

simpleOBJFile file texturefile = Fix $ Invoke $
          (#shader =: "unlit3d")
      :&  (#staticParameters =: ((#tex =: (0 :: GLint)) :& RNil) )
      :&  (#frameParameters =: (
                                    (#cameraProjection =: (identity :: M44 GLfloat))
                                 :& (#worldTransform =: (identity :: M44 GLfloat))
                                 :& (#curTime =: (0 :: GLfloat))
                                 :& RNil )
          )
      :&  (#vertexBuffers =: [ OBJFile file ]
          )
      :&  (#textures =: [texturefile])
      :&  RNil
    

test3DScene = perspective3DView (1,20) $ 
                translate3d (V3 (0.5) (-0.5) (-4)) $
                  rotate3dDynamic (V3 0 1 1) 0.3 $
                    translate3d (V3 (-0.5) (-0.5) (-0.5)) $
                      simpleOBJFile "testcube.obj" "salamander.png"

-}

genRenderParams :: AppInfo -> Rec TopLevel2DRow
genRenderParams appstate =
  let t     = appstate .! #curTime
      (w,h) = appstate .! #windowSize
  in   (#modelViewMatrix .== (identity :: M44 Float)) .+
       (#projectionMatrix .== (identity :: M44 Float)) .+
       (#curTime .== t) .+
       (#windowX .== fromIntegral w) .+
       (#windowY .== fromIntegral h)

--testScene :: SceneGraph TopLevel2DRow OpenGLTarget
testScene :: SceneGraph TopLevel2DRow OpenGLTarget
testScene = pixelOrtho2DView $
              translate2d (V2 20 20) $
                group [
                  invoke (#shader .== (ShaderFiles "linez.vert" "linez.frag") .+
                          #vertices .== (RawV2 [V2 0 0, V2 10 0, V2 10 10, V2 0 0, V2 0 10, V2 10 10]))
                  ]

{- pixelOrtho2DView $
              group
              [
                translate2d (V2 0 0)    $ simpleSquare "sad-crab.png",
                translate2d (V2 150 50) $ simpleSquare "owl.png"
              ]
-}

main :: IO ()
main = simpleApp (600,400) (const testScene)
