{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs, TypeSynonymInstances, TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE OverloadedLabels, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Fomorian.Sample (main2d, main3d, oneSceneApp) where


import Linear
import Data.Vinyl

import Data.Functor.Foldable
import Control.Exception

import Graphics.Rendering.OpenGL as GL

import qualified Fomorian.Windowing as W

import Fomorian.SceneNode
import Fomorian.SceneResources
import Fomorian.Common

--
-- 2d draw test
--

simpleSquare :: forall k (cmd :: k) np. 
  ShaderReady cmd (FieldRec '["tex" ::: GLint]) => 
    String ->
    Fix (SceneNode (FieldRec '["cameraProjection" ::: M44 GLfloat, "worldTransform" ::: M44 GLfloat, "curTime" ::: GLfloat]) np cmd)
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


--
-- 3d draw test
--

simpleOBJFile :: forall k (cmd :: k) np.
  (ShaderReady cmd (FieldRec '["tex" ::: GLint])) =>
    String ->
    String ->
    Fix (SceneNode StandardShaderFrameParams np cmd)
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
    


genRenderParams :: W.AppInfo s -> TopWindowFrameParams
genRenderParams appInfo =
  let (w,h) = rvalf #windowSize appInfo
      t     = rvalf #curTime appInfo
  in   (#windowX =: fromIntegral w)
    :& (#windowY =: fromIntegral h)
    :& (#curTime =: t)
    :& RNil



main2d :: IO ()
main2d = let testScene = pixelOrtho2DView $
                group
                [
                  translate2d (V2 0 0)    $ simpleSquare "sad-crab.png",
                  translate2d (V2 250 50) $ simpleSquare "owl.png"
                ]
          in
            oneSceneApp testScene

main3d :: IO ()
main3d = let test3DScene = perspective3DView (1,20) $ 
                translate3d (V3 (0.5) (-0.5) (-4)) $
                  rotate3dDynamic (V3 0 1 1) 0.3 $
                    translate3d (V3 (-0.5) (-0.5) (-0.5)) $
                      simpleOBJFile "testcube.obj" "salamander.png"
          in
            oneSceneApp test3DScene

data EmptyAppState = EmptyAppState

oneSceneApp :: SceneGraph (FieldRec '[]) TopWindowFrameParams DrawGL -> IO ()
oneSceneApp scene = do
  let windowConfig = (600,400,"Demo")
  let initfunc = W.initWindow windowConfig >>= return
  let endfunc  = \win -> W.terminateWindow win
  {- let runForOneSecond = \appinfo -> let t = rgetf #curTime appinfo
                                    in
                                      if t < 1.0 then W.NextFrame else W.EndApp -}
  let runForever = \_ -> (EmptyAppState, W.NextFrame)
  let loopfunc = \win -> do
                           appdata <- W.initAppState windowConfig win EmptyAppState
                           W.renderLoop appdata (const scene) genRenderParams runForever
  bracket initfunc endfunc loopfunc

