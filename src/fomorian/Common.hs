{-# LANGUAGE DataKinds, PolyKinds, 

TypeOperators,
TypeFamilies,
FlexibleContexts, 
FlexibleInstances, 
NoMonomorphismRestriction,

GADTs, TypeSynonymInstances, TemplateHaskell, OverloadedLabels,

StandaloneDeriving,
RankNTypes

#-}

-- | Commonly used nodes you might use to quickly rendering some
-- | 2d or 3d objects.  This covers the typical cases of rendering
-- | 2d 'sprites' or unlit 3d meshes, along with common spatial
-- | transforms like translation or rotation.
-- |


module Fomorian.Common where

import Linear

import Graphics.Rendering.OpenGL (GLfloat, GLint)

import Data.Kind (Constraint)
import Data.Functor.Foldable

-- vinyl
import Data.Vinyl
import Data.Vinyl.Lens
import Data.Vinyl.TypeLevel (Nat(Z),Nat(S), AllConstrained)
import Data.Vinyl.Functor
import qualified Data.Constraint as DC

import Fomorian.SceneNode

--
-- transformers
--

type TopWindowFrameFields = '[ '("windowX", Integer), '("windowY",Integer), '("curTime",Float) ]
type TopWindowFrameParams = FieldRec TopWindowFrameFields

buildPixelOrthoMatrix :: (Integral a, RealFrac b) => a -> a -> M44 b
buildPixelOrthoMatrix w h =
  let 
    w1 = 1.0 / fromIntegral w
    h1 = 1.0 / fromIntegral h
    scaleMatrix x y z = V4  (V4 x 0 0 0)
                            (V4 0 y 0 0)
                            (V4 0 0 z 0)
                            (V4 0 0 0 1)
    translationMatrix x y z = V4 (V4 1 0 0 x)
                                  (V4 0 1 0 y)
                                  (V4 0 0 1 z)
                                  (V4 0 0 0 1)
    m1 = translationMatrix (-1) (-1) 0
    m2 = scaleMatrix (w1*2) (h1*2) 1
  in
    m1 !*! m2



orthize :: (fr ~ FieldRec ff, TopWindowFrameFields <: ff) => fr -> M44 GLfloat
orthize d = let rf = rcast d :: FieldRec TopWindowFrameFields
                w = rvalf #windowX rf
                h = rvalf #windowY rf
            in
              buildPixelOrthoMatrix w h

type StandardShaderFrameFields = '[
    '("cameraProjection", M44 GLfloat),
    '("worldTransform", M44 GLfloat),
    '("curTime", Float)
  ]
type StandardShaderFrameParams = FieldRec StandardShaderFrameFields
  
orthoConvert :: (ShaderReady cmd StandardShaderFrameParams) =>
  FrameData sp TopWindowFrameParams cmd ->
  FrameData StandardShaderFrameParams (FieldRec '[]) cmd
orthoConvert (FrameData sp np dc) = 
        let orthoM = orthize np
            t = rvalf #curTime np
            frameData =      (#cameraProjection =: orthoM)
                          :& (#worldTransform =: (identity :: M44 GLfloat) )
                          :& (#curTime =: t)
                          :& RNil
        in
          DC.withDict dc $ FrameData frameData RNil DC.Dict

--
-- | Generate an orthographic projection where OpenGL coordinates
-- | are mapped onto window pixels. For a window of width $w$ and
-- | height $h$ the coordinates (0,0) are the lower-left corner of the
-- | window and (w,h) refer to the upper-right corner of the window.
-- | Resizing the window will change these values; if you want some sort
-- | of auto-scaling with the window use $ortho2DView$ or $fitOrtho2DView$
--
pixelOrtho2DView :: (fr ~ FieldRec ff,
                     StandardShaderFrameFields ~ ff,
                     ShaderReady cmd StandardShaderFrameParams,
                     np ~ FieldRec nf) =>
  SceneGraph StandardShaderFrameParams (FieldRec '[]) cmd ->
  Fix (SceneNode sp TopWindowFrameParams cmd)
pixelOrtho2DView sg = Fix $ Transformer orthoConvert sg


--
--
--
translateWorld :: (ShaderReady cmd StandardShaderFrameParams) => 
  V3 GLfloat ->
  FrameData StandardShaderFrameParams np cmd ->
  FrameData StandardShaderFrameParams np cmd
translateWorld (V3 tx ty tz) (FrameData sp np dc) =
  let xform = rvalf #worldTransform sp
      t     = rvalf #curTime sp
      translationMatrix x y z = V4 (V4 1 0 0 x)
                                   (V4 0 1 0 y)
                                   (V4 0 0 1 z)
                                   (V4 0 0 0 1)
      xform' = xform !*! translationMatrix tx ty tz
      cproj = rvalf #cameraProjection sp
      frameData =    (#cameraProjection =: cproj)
                  :& (#worldTransform =: xform')
                  :& (#curTime =: t)
                  :& RNil
  in
    FrameData frameData np DC.Dict

translate2d :: (ShaderReady cmd StandardShaderFrameParams, np ~ FieldRec nf) =>
  V2 GLfloat ->
  SceneGraph StandardShaderFrameParams np cmd ->
  Fix (SceneNode StandardShaderFrameParams np cmd)
translate2d (V2 tx ty) sg = Fix $ Transformer (translateWorld (V3 tx ty 0)) sg

translate3d :: (ShaderReady cmd StandardShaderFrameParams, np ~ FieldRec nf) =>
  V3 GLfloat ->
  SceneGraph StandardShaderFrameParams np cmd ->
  Fix (SceneNode StandardShaderFrameParams np cmd)
translate3d tr sg = Fix $ Transformer (translateWorld tr) sg


