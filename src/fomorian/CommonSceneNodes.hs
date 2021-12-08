{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-| Commonly used nodes you might use to quickly render some
    2d or 3d objects.  This covers the typical cases of rendering
    2d 'sprites' or unlit 3d meshes, along with common spatial
    transforms like translation or rotation.
-}
module Fomorian.CommonSceneNodes where

import Linear

import Graphics.Rendering.OpenGL (GLfloat)

import Data.Row
import Data.Row.Records as R

import Fomorian.SceneNode

--
-- transformers
--

-- | generate a 'M44' to scale in x,y,z
scaleMatrix :: (Num a) => a -> a -> a -> M44 a
scaleMatrix x y z = scaled (V4 x y z 1)

-- | generate a 'M44' to translate by x,y,z
translationMatrix :: (Num a) => a -> a -> a -> M44 a
translationMatrix x y z = V4 (V4 1 0 0 x)
                             (V4 0 1 0 y)
                             (V4 0 0 1 z)
                             (V4 0 0 0 1)

-- | Build a matrix to map from from a rectangle [ (0,0) - (w,h) ] to [ (-1,-1) - (1,1) ]
buildPixelOrthoMatrix :: (Integral a, RealFrac b) => a -> a -> M44 b
buildPixelOrthoMatrix w h =
  let 
    w1 = 1.0 / fromIntegral w
    h1 = 1.0 / fromIntegral h
    m1 = translationMatrix (-1) (-1) 0
    m2 = scaleMatrix (w1*2) (h1*2) 1
  in
    m1 !*! m2

-- | Generate ortho matrix given a window size in pixels.
orthoForWindowSize :: (HasType "windowX" Integer r, HasType "windowY" Integer r) => Rec r -> M44 GLfloat
orthoForWindowSize rf =
  let w = rf .! #windowX
      h = rf .! #windowY
  in
      buildPixelOrthoMatrix w h

-- | Generate record entry for a projectionMatrix given window size
orthoConvert :: (HasType "windowX" Integer r, HasType "windowY" Integer r) =>
  Rec r -> Rec ("projectionMatrix" .== (M44 Float))
orthoConvert r = (#projectionMatrix .== orthoForWindowSize r)

--
-- | Generate an orthographic projection where OpenGL coordinates
-- | are mapped onto window pixels. For a window of width $w$ and
-- | height $h$ the coordinates (0,0) are the lower-left corner of the
-- | window and (w,h) refer to the upper-right corner of the window.
-- | Resizing the window will change these values; if you want some sort
-- | of auto-scaling with the window use $ortho2DView$ or $fitOrtho2DView$
--
pixelOrtho2DView :: (HasType "windowX" Integer dr, HasType "windowY" Integer dr) =>
  SceneGraph target (("projectionMatrix" .== (M44 Float)) .// dr) -> SceneGraph target dr
pixelOrtho2DView sg = setFields orthoConvert sg

data PerspectiveProjectConfig =
  PerspectiveProject
  {
    fov :: Float,
    aspect :: Float,
    nearPlane :: Float,
    farPlane :: Float
  } deriving (Eq, Show)


-- | Generate a perspective projection representing the camera looking down the -z axis from the origin.
perspectiveProject :: PerspectiveProjectConfig -> SceneGraph target ("projectionMatrix" .== M44 Float .// dr) -> SceneGraph target dr
perspectiveProject (PerspectiveProject v a n f) sg = setFields setPerspective sg
  where
    setPerspective :: Rec r -> Rec ("projectionMatrix" .== M44 Float)
    setPerspective _ = (#projectionMatrix .== perspective v a n f)

autoAspect :: (HasType "windowX" Integer dr, 
               HasType "windowY" Integer dr, 
               HasType "projectionMatrix" (M44 Float) dr,
               HasType "correctNDC" (M44 Float) dr) =>
  SceneGraph target dr -> SceneGraph target dr
autoAspect sg = Transformer correctAspect sg
  where
    correctAspect r =
      let x = r .! #windowX
          y = r .! #windowY
          a = ((fromInteger y)/(fromInteger x)) :: Float
          m = r .! #projectionMatrix
          scaleAspect = scaleMatrix a 1 1
          correctNDC = r .! #correctNDC
      in update #projectionMatrix (scaleAspect !*! m !*! correctNDC) r

cameraLookAt :: (HasType "viewMatrix" (M44 Float) dr) => V3 Float -> V3 Float -> V3 Float -> SceneGraph target dr -> SceneGraph target dr
cameraLookAt camat lookat upvector sg = Transformer setLookAt sg
  where
    setLookAt r = update #viewMatrix (lookAt camat lookat upvector) r

-- | Update 'modelMatrix' field to perform a translation
translateBy :: (HasType "modelMatrix" (M44 Float) r) => V3 Float -> Rec r -> Rec r
translateBy (V3 tx ty tz) r =
  let xform = r .! #modelMatrix
      xform' = xform !*! (translationMatrix tx ty tz)
  in
    update #modelMatrix xform' r


-- | Applies a 2 dimensional translation to the modelMatrix frame parameter.
translate2d :: (HasType "modelMatrix" (M44 Float) dr) => V2 Float -> SceneGraph target dr -> SceneGraph target dr
translate2d (V2 tx ty) sg = Transformer (translateBy (V3 tx ty 0)) sg

-- | Applies a 3 dimensional translation to the modelMatrix frame parameter
translate3d :: (HasType "modelMatrix" (M44 Float) dr) => V3 Float -> SceneGraph target dr -> SceneGraph target dr
translate3d tr sg = Transformer (translateBy tr) sg

-- | Applies a time-varying translation. Uses a function that takes in the current time and returns the translation for that time.
translateWithFunc :: (HasType "curTime" Float dr, HasType "modelMatrix" (M44 Float) dr) => (Float -> V3 Float) -> SceneGraph target dr -> SceneGraph target dr
translateWithFunc f sg = Transformer (translateFunc f) sg
  where
    translateFunc ft = \r -> let t = r .! #curTime
                                 (V3 dx dy dz) = ft t
                                 xform = r .! #modelMatrix
                                 xform' = xform !*! (translationMatrix dx dy dz)
                             in
                               update #modelMatrix xform' r


rotateAxisAngle :: (HasType "modelMatrix" (M44 Float) r) => V3 GLfloat -> Float ->
  Rec r -> Rec r
rotateAxisAngle axis ang r =
  let xform = r .! #modelMatrix
      -- first build a quaterion, then convert to a matrix
      quat = axisAngle axis ang
      rotationMatrix = mkTransformation quat (V3 0 0 0)
      xform' = xform !*! rotationMatrix
  in update #modelMatrix xform' r

rotate3d :: (HasType "modelMatrix" (M44 Float) dr) => V3 GLfloat -> Float -> SceneGraph target dr -> SceneGraph target dr
rotate3d axis ang = transformer (rotateAxisAngle axis ang)

spin3d :: (HasType "curTime" Float dr, HasType "modelMatrix" (M44 Float) dr) => V3 GLfloat -> Float -> SceneGraph target dr -> SceneGraph target dr
spin3d axis spinspeed = transformer (spinAxis spinspeed)
  where
    spinAxis spin r =
      let ang = spin * realToFrac (r .! #curTime)
      in rotateAxisAngle axis ang r

-- | Update 'modelMatrix' field to perform a translation
scaleBy :: (HasType "modelMatrix" (M44 Float) r) => V3 Float -> Rec r -> Rec r
scaleBy (V3 sx sy sz) r =
  let xform = r .! #modelMatrix
      xform' = xform !*! scaleMatrix sx sy sz
  in
    update #modelMatrix xform' r

scale3d :: (HasType "modelMatrix" (M44 Float) dr) => V3 GLfloat -> SceneGraph target dr -> SceneGraph target dr
scale3d scaleVals = transformer (scaleBy scaleVals)

