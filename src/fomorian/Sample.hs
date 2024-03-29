{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Fomorian.Sample where


import Linear
import Data.Row
import Data.Text (Text)
import Data.Map as M

import Fomorian.SceneNode
import Fomorian.SimpleApp
import Fomorian.NeutralSceneTarget
import Fomorian.SceneResources
import Fomorian.CommonSceneNodes

import Fomorian.PlatformRenderer
import Fomorian.OpenGL.OpenGLResources


genRenderParams :: AppInfo x -> Rec TopLevel3DRow
genRenderParams appstate =
  let t     = appstate .! #curTime
      (w,h) = appstate .! #windowSize
  in   (#modelMatrix .== (identity :: M44 Float)) .+
       (#viewMatrix .== (identity :: M44 Float)) .+
       (#projectionMatrix .== (identity :: M44 Float)) .+
       (#curTime .== t) .+
       (#windowX .== fromIntegral w) .+
       (#windowY .== fromIntegral h)



prebuiltResources :: Map Text BasicResource
prebuiltResources = M.fromList [
  ("square", Resource (IsJust #vertexPositions $ vertex2ToGeometry [(0,0), (10,0), (10,10), (0, 0), (0, 10), (10, 10)]))
  ]


testScene2d :: SceneGraph NeutralSceneTarget DefaultDrawFrameParams 
testScene2d = neutral3DSceneRoot $
                pixelOrtho2DView $
                  group [
                    someTriangle,
                    translateWithFunc sinFunc someTriangle
                    ]
  where
    sinFunc t = V3 (10 * (sin t)) (10 * (cos t)) 0
    someTriangle :: (DrawReq NeutralSceneTarget dr) => SceneGraph NeutralSceneTarget dr
    someTriangle = invoke (  #shader   .== "linez"
                          .+ #geometry .== DataSource (IsJust #userSource "square")
                          .+ #textures .== [])

testScene3d :: SceneGraph NeutralSceneTarget DefaultDrawFrameParams 
testScene3d = neutral3DSceneRoot $
                perspectiveProject config $
                  autoAspect $
                    cameraLookAt (V3 5 10 0) (V3 0 0 0) (V3 0 0 1) $ 
                      group [
                        someCube,
                        translate3d (V3 3 0 0) $ spin3d (V3 0.7071 0.7071 0) 2 someCube
                        ]
  where
    config = PerspectiveProject  1.2 {-fov-} 1.0 {-aspect-} 0.1 {-near plane-} 1000 {-far plane-}
    someCube :: (DrawReq NeutralSceneTarget dr) => SceneGraph NeutralSceneTarget dr
    someCube = wavefrontMesh "unlit3d" "testcube.obj" ["salamander.png"]

dynamicsTestScene3d :: SceneGraph NeutralSceneTarget DefaultDrawFrameParams
dynamicsTestScene3d = undefined

main :: IO ()
main = simpleApp (600,400) (const (neutralToGLTarget testScene3d))
