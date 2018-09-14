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
import Control.Lens ((^.), (.~))
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
                      '("worldTransform", M44 GLfloat)
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



testScene :: (SceneSYM repr, InvokeConstraint repr OrthoParams) => repr (FieldRec PixelOrthoFrameFields)
testScene = ortho2DView $ group [
              translate2d (0,0) $ simpleSquare "owl.png",
              translate2d (100,100) $ simpleSquare "sad-crab.png"
              ]
         
genRenderParams :: W.AppInfo -> (FieldRec PixelOrthoFrameFields)
genRenderParams appstate = let (w,h) = rvalf #windowSize appstate
  in   (#windowX =: fromIntegral w)
    :& (#windowY =: fromIntegral h)
    :& RNil



--
renderLoop ::
  IORef W.AppInfo -> 
  (forall scenerepr . (SceneSYM scenerepr, InvokeConstraint scenerepr OrthoParams) => W.AppInfo -> scenerepr (fr)) -> (W.AppInfo -> fr) ->
  IO ()
renderLoop appref buildScene genRP = loop
  where
    loop = do
        appstate <- readIORef appref
        let win = rvalf #window appstate
        let resources = rvalf #resources appstate
        let needresources = needsGLResources $ buildScene appstate
        new_resources <- loadResources needresources resources
        let new_appstate = (rputf #resources new_resources $ appstate)

        let frame_data = genRP new_appstate
        let scene = buildScene appstate
        renderApp new_appstate scene frame_data
        writeIORef appref new_appstate

        GLFW.swapBuffers win
        shouldClose <- W.shouldEndProgram win
        unless shouldClose loop

renderApp :: W.AppInfo -> DrawGL IO (fr) -> fr -> IO ()
renderApp appstate scene framedata = do
  GL.clearColor $= Color4 0 0 0 0
  GL.clear [GL.ColorBuffer]
  let resourceMap = rvalf #resources appstate
  let drawGO = runDrawGL scene
  renderresult <- try $ drawGO framedata resourceMap
  case renderresult of
    Left e -> do
      putStrLn $ displayException (e :: SomeException)
    Right () -> return ()
      

main :: IO ()
main = do
  let scene = testScene
  let windowConfig = (400,400,"Demo")
  win <- W.initWindow windowConfig
  appdata <- W.initAppState windowConfig win
  renderLoop appdata (const scene) genRenderParams
  W.terminateWindow win
