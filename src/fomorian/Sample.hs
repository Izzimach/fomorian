{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
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


import Fomorian.SceneNode
import Fomorian.SimpleApp
import Fomorian.NeutralSceneTarget
import Fomorian.SceneResources
import Fomorian.OpenGLResources
import Fomorian.CommonSceneNodes


genRenderParams :: AppInfo -> Rec TopLevel3DRow
genRenderParams appstate =
  let t     = appstate .! #curTime
      (w,h) = appstate .! #windowSize
  in   (#modelMatrix .== (identity :: M44 Float)) .+
       (#viewMatrix .== (identity :: M44 Float)) .+
       (#projectionMatrix .== (identity :: M44 Float)) .+
       (#curTime .== t) .+
       (#windowX .== fromIntegral w) .+
       (#windowY .== fromIntegral h)





testScene2d :: SceneGraph NeutralSceneTarget TopLevel3DRow 
testScene2d = pixelOrtho2DView $
                group [
                  someTriangle,
                  translateWithFunc sinFunc someTriangle
                  ]
  where
    sinFunc t = V3 (10 * (sin t)) (10 * (cos t)) 0
    someTriangle :: SceneGraph NeutralSceneTarget TopLevel3DRow
    someTriangle = invoke (  #shader   .== "linez"
                          .+ #geometry .== DataSource (IsJust #coordinates2d [(0,0), (10,0), (10,10), (0, 0), (0, 10), (10, 10)])
                          .+ #textures .== [])

testScene3d :: SceneGraph NeutralSceneTarget TopLevel3DRow 
testScene3d = perspectiveProject config $
                -- We set the static aspect in 'PerspectiveProject' to 1.0 and let 'autoAspect' handle
                -- the aspect to work with window resizing.
                autoAspect $
                  cameraLookAt (V3 5 10 0) (V3 0 0 0) (V3 0 0 1) $ 
                    group [
                      someCube,
                      translate3d (V3 3 0 0) $ spin3d (V3 0.7071 0.7071 0) 2 $ someCube
                      ]
  where
    config = (PerspectiveProject  1.2 {-fov-} 1.0 {-aspect-} 0.1 {-near plane-} 1000 {-far plane-})
    someCube :: SceneGraph NeutralSceneTarget TopLevel3DRow
    someCube = wavefrontMesh "unlit3d" "testcube.obj" ["salamander.png"]

dynamicsTestScene3d :: SceneGraph NeutralSceneTarget TopLevel3DRow
dynamicsTestScene3d = undefined

main :: IO ()
main = simpleApp (600,400) (const (neutralToGLTarget testScene3d))
