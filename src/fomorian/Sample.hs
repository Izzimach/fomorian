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
import Data.Int
import Foreign.Ptr


import Fomorian.SceneNode
import Fomorian.SimpleApp
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


testScene2d :: SceneGraph TopLevel3DRow OpenGLTarget
testScene2d = pixelOrtho2DView $
                group [
                  someTriangle,
                  translateWithFunc sinFunc someTriangle
                  ]
  where
    sinFunc t = V3 (10 * (sin t)) (10 * (cos t)) 0
    someTriangle :: SceneGraph TopLevel3DRow OpenGLTarget
    someTriangle = invoke (  #geometry .== ("linez", DataSource (IsJust #coordinates2d [(0,0), (10,0), (10,10), (0, 0), (0, 10), (10, 10)]))
                          .+ #textures .== [])

testScene3d :: SceneGraph TopLevel3DRow OpenGLTarget
testScene3d = perspectiveProject config $
                -- We set the static aspect in 'PerspectiveProject' to 1.0 and let 'autoAspect' handle
                -- the aspect to work with window resizing.
                autoAspect $
                  cameraLookAt (V3 0 5 5) (V3 0 0 0) (V3 0 0 1) $ 
                    group [
                      someCube,
                      translate3d (V3 4 0 0) $ someCube
                      ]
  where
    config = (PerspectiveProject  1.2 {-fov-} 1.0 {-aspect-} 0.1 {-near plane-} 100 {-far plane-})
    someCube :: SceneGraph TopLevel3DRow OpenGLTarget
    someCube = invoke (   #geometry .== ("unlit3d", DataSource (IsJust #wavefrontPath "testcube.obj"))
                       .+ #textures .== [DataSource (IsJust #texturePath "salamander.png")])

myAdd :: Int32 -> Int32
myAdd x = x + 3


main :: IO ()
main = simpleApp (600,400) (const testScene3d)
