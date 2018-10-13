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
import qualified Graphics.UI.GLFW as GLFW

import Linear
import Data.Vinyl
import Data.Word (Word32)
import qualified Data.Constraint as DC
import Graphics.VinylGL

import Data.IORef
import Data.Maybe (mapMaybe)
import Data.Bifunctor (bimap)
import Control.Monad
import Control.Lens ((^.), (.~), (%~))
import Control.Exception
import Control.Monad.Trans
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import System.FilePath ((</>))

import qualified Windowing as W

import SceneFinal
import SceneResources


squareRPProxy :: RenderParamsProxy (InvokableFrameData repr 
                    ('[
                      '("cameraProjection", M44 GLfloat),
                      '("worldTransform", M44 GLfloat),
                      '("time", Float)
                    ])
                  )
squareRPProxy = RPProxy

simpleSquare file = invoke $ Invocation $
          (#shader =: "linez")
      :&  (#shaderParameters =: ((#tex =: (0 :: GLint)) :& RNil) )
      :&  (#vertexBuffers =: [
              V2Data [V2 0 0, V2 100 0, V2 0 100, V2 100 100],
              T2Data [V2 0 1, V2 1 1, V2 0 0, V2 1 0],
              IndexData [0,1,2, 2,1,3]
            ]
          )
      :&  (#textures =: [file])
      :&  (#rpProxy =: squareRPProxy)
      :&  RNil

simpleOBJFile file texturefile = invoke $ Invocation $
          (#shader =: "unlit3d")
      :&  (#shaderParameters =: ((#tex =: (0 :: GLint)) :& RNil) )
      :&  (#vertexBuffers =: [ OBJFile file ]
          )
      :&  (#textures =: [texturefile])
      :&  (#rpProxy =: squareRPProxy)
      :&  RNil
    

testScene :: (SceneSYM repr, InvokeConstraint repr OrthoParams) => repr (FieldRec PixelOrthoFrameFields)
testScene = pixelOrtho2DView $ group [
              translate2d (V2 0 0)$ simpleSquare "owl.png",
              translate2d (V2 100 100) $ simpleSquare "sad-crab.png"
              ]

test3DScene :: (SceneSYM repr, InvokeConstraint repr OrthoParams, fr ~ FieldRec ff, PixelOrthoFrameFields ~ ff) => repr fr
test3DScene = perspective3DView (1,20) $ 
                translate3d (V3 (0.5) (-0.5) (-4)) $
                  rotate3dDynamic (V3 0 1 1) 0.3 $
                    translate3d (V3 (-0.5) (-0.5) (-0.5)) $
                      simpleOBJFile "testcube.obj" "salamander.png"

genRenderParams :: W.AppInfo -> FieldRec ['("windowX", Integer), '("windowY", Integer), '("curTime", Float)]
genRenderParams appstate =
  let (w,h) = rvalf #windowSize appstate
      t = rvalf #curTime appstate
  in   (#windowX =: fromIntegral w)
    :& (#windowY =: fromIntegral h)
    :& (#curTime =: t)
    :& RNil



--
renderLoop ::
  IORef W.AppInfo -> 
  (forall scenerepr . (SceneSYM scenerepr, InvokeConstraint scenerepr OrthoParams) => W.AppInfo -> scenerepr fr) ->
  (W.AppInfo -> fr) ->
  IO ()
renderLoop appref buildScene genRP = loop
  where
    loop = do
        appstate <- readIORef appref
        let win = rvalf #window appstate
        let resources = rvalf #resources appstate
        let needresources = needsGLResources $ buildScene appstate
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

renderApp :: ResourceMap -> DrawGL IO fr -> fr -> IO ()
renderApp resources scene framedata = do
  GL.clearColor $= Color4 0.1 0.1 0.1 0
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  depthFunc $= Just Less
  cullFace $= Just Front
  renderresult <- try $ runDrawGL scene framedata resources
  case renderresult of
    Left e   -> putStrLn $ displayException (e :: SomeException)
    Right () -> return ()
      

main :: IO ()
main = do
  let scene = test3DScene
  let windowConfig = (600,400,"Demo")
  let initfunc = W.initWindow windowConfig >>= return
  let endfunc  = \win -> W.terminateWindow win
  let loopfunc = \win -> do
                           appdata <- W.initAppState windowConfig win
                           renderLoop appdata (const scene) genRenderParams
  bracket initfunc endfunc loopfunc
