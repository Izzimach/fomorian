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


import Fomorian.SceneNode
import Fomorian.SimpleApp
import Fomorian.SceneResources
import Fomorian.OpenGLResources
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
    someTriangle = invoke (#shader   .== MaterialData (ShaderPath "linez") .+
                           #vertices .== GeometryData (RawV2 [V2 0 0, V2 10 0, V2 10 10, V2 0 0, V2 0 10, V2 10 10]) .+
                           #textures .== [])

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
    someCube = invoke (#shader .== MaterialData (ShaderPath "unlit3d") .+
                       #vertices .== GeometryData (OBJFile "testcube.obj") .+
                       #textures .== ([MaterialData (TexturePath "salamander.png")]))

main :: IO ()
main = simpleApp (600,400) (const testScene3d)
