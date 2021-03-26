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

myAdd :: Int32 -> Int32
myAdd x = x + 3

foreign import ccall unsafe "foo" hFoo :: Int32 -> IO Int32
foreign import ccall safe "foo2" hFoo2 :: FunPtr (Int32 -> Int32) -> IO Int32
foreign import ccall "wrapper" createAddPtr :: (Int32 -> Int32) -> IO (FunPtr (Int32 -> Int32))


main2 :: IO ()
main2 = simpleApp (600,400) (const testScene3d)

main :: IO ()
main = do
  x <- hFoo 41
  putStrLn (show x)
  myAddPtr <- createAddPtr myAdd
  x2 <- hFoo2 myAddPtr
  putStrLn (show x2)
  freeHaskellFunPtr myAddPtr