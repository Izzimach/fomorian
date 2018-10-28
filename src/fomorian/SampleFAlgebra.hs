{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs, TypeSynonymInstances, TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE OverloadedLabels, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Fomorian.SampleFAlgebra where


import Linear
import Data.Vinyl
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
import Graphics.VinylGL


import qualified Fomorian.Windowing as W

import Fomorian.SceneFAlgebra
import Fomorian.SceneResources

--simpleSquare :: String -> Invocation sp
simpleSquare file = Invoke $ 
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
              IndexData [0,1,2, 2,1,3]
            ]
          )
      :&  (#textures =: [file])
      :&  RNil

testScene = Fix $ pixelOrtho2DView $
              Fix $ Group
              [
                Fix $ (simpleSquare "owl.png"),
                Fix $ (simpleSquare "sad-crab.png")
              ]


genRenderParams :: W.AppInfo -> TopWindowFrameParams
genRenderParams appstate =
  let (w,h) = rvalf #windowSize appstate
      t     = rvalf #curTime appstate
  in   (#windowX =: fromIntegral w)
    :& (#windowY =: fromIntegral h)
    :& (#curTime =: t)
    :& RNil

renderApp ::
  ResourceMap ->
  SceneGraph (FieldRec '[]) TopWindowFrameParams DrawGL ->
  TopWindowFrameParams ->
  IO ()
renderApp resources scene windowparams = do
  let framedata = FrameData RNil windowparams DC.Dict
  GL.clearColor $= Color4 0.1 0.1 0.1 0
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  depthFunc $= Just Less
  cullFace $= Just Front
  renderresult <- try $ openGLgo scene framedata resources
  case renderresult of
    Left e   -> putStrLn $ displayException (e :: SomeException)
    Right () -> return ()
      


renderLoop ::
  IORef W.AppInfo -> 
  (W.AppInfo -> SceneGraph (FieldRec '[]) TopWindowFrameParams DrawGL) ->
  (W.AppInfo -> TopWindowFrameParams) ->
  IO ()
renderLoop appref buildScene genRP = loop
  where
    loop = do
        appstate <- readIORef appref
        let win = rvalf #window appstate
        let resources = rvalf #resources appstate
        let needresources = oglResourcesScene $ buildScene appstate
        new_resources <- loadResources needresources resources
        let bumpTime = (rlensf #curTime) %~ (+0.016)
        let new_appstate = bumpTime . (rputf #resources new_resources) $ appstate

        let frame_data = genRP new_appstate
        let scene = buildScene appstate
        renderApp new_resources scene frame_data
        writeIORef appref new_appstate

        GLFW.swapBuffers win
        shouldClose <- W.shouldEndProgram win
        unless shouldClose loop



main :: IO ()
main = do
  let scene = testScene
  let windowConfig = (600,400,"Demo")
  let initfunc = W.initWindow windowConfig >>= return
  let endfunc  = \win -> W.terminateWindow win
  let loopfunc = \win -> do
                           appdata <- W.initAppState windowConfig win
                           renderLoop appdata (const scene) genRenderParams
  bracket initfunc endfunc loopfunc

