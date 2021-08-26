{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} {- oh no -}

{-|
The 'NeutralSceneTarget' is meant to be a generic target that describes stuff to draw without having
any hardware or API-specific reference. The constraints here only require some simple values to draw stuff.
-}
module Fomorian.NeutralSceneTarget where

import Data.Row

import Linear

import Fomorian.SceneNode
import Fomorian.SceneResources

-- | NeutralSceneTarget requires
--   - (shader,geometry,textures) for invoke and
--   - (modelMAtrix,viewMatrix,projectionMatrix) for draw
data NeutralSceneTarget

type instance (InvokeReq NeutralSceneTarget ir) =
  (
    HasType "shader" FilePath ir,
    HasType "geometry" (DataSource BasicDataSourceTypes) ir,
    HasType "textures" [DataSource BasicDataSourceTypes] ir
  )


type instance (DrawReq NeutralSceneTarget dr) =
  (
    HasType "modelMatrix" (M44 Float) dr,
    HasType "viewMatrix" (M44 Float) dr,
    HasType "projectionMatrix" (M44 Float) dr
  )


