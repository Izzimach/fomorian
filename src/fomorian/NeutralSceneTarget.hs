{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGe TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-} {- oh no -}

{-|
The 'NeutralSceneTarget' is meant to be a generic target that describes stuff to draw without having
any hardware or API-specific reference. The constraints here only require some simple values to draw stuff.
-}
module Fomorian.NeutralSceneTarget where

import Data.Row

import Linear

import Fomorian.SceneNode
import Fomorian.SceneResources ( BasicDataSource, DataSource(DataSource) )


-- | NeutralSceneTarget requires
--   - (shader,geometry,textures) for invoke and
--   - (modelMAtrix,viewMatrix,projectionMatrix) for draw
data NeutralSceneTarget

type instance (InvokeReq NeutralSceneTarget ir) =
  (
    HasType "shader" FilePath ir,
    HasType "geometry" BasicDataSource ir,
    HasType "textures" [BasicDataSource] ir
  )


type instance (DrawReq NeutralSceneTarget dr) =
  (
    HasType "modelMatrix" (M44 Float) dr,
    HasType "viewMatrix" (M44 Float) dr,
    HasType "projectionMatrix" (M44 Float) dr
  )

type NeutralSceneFields = ("modelMatrix" .== (M44 Float) .+ "viewMatrix" .== (M44 Float) .+ "projectionMatrix" .== (M44 Float))

-- | Injects the fields required by NeutralSceneTarget for a 3D scene into the subtree. This include the projection matrix and the model/view matrices.
--   Matrices are initialized as identity matrices. Use projection or transformation nodes to modify them.
neutral3DSceneRoot ::
  SceneGraph NeutralSceneTarget (NeutralSceneFields .// dr) -> 
  SceneGraph NeutralSceneTarget dr
neutral3DSceneRoot sg = setFields neutralFields sg
  where
    neutralFields :: (Rec r -> Rec NeutralSceneFields)
    neutralFields _ = let identM = (identity :: M44 Float)
                      in (#modelMatrix .== identM .+
                          #viewMatrix .== identM .+
                          #projectionMatrix .== identM)


-- | Generate an invoke node with all the fields for neutral target, using a wavefront file for geometry
wavefrontMesh :: (DrawReq NeutralSceneTarget dr) => FilePath -> FilePath -> [FilePath] -> SceneGraph NeutralSceneTarget dr
wavefrontMesh sh wf texs =
  invoke (   #shader   .== sh
          .+ #geometry .== DataSource (IsJust #wavefrontPath wf)
          .+ #textures .== fmap (\t -> DataSource (IsJust #texturePath t)) texs
         )



