{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} {- oh no -}

module Fomorian.NeutralSceneTarget where

import Data.Row

import Linear

import Fomorian.SceneNode
import Fomorian.SceneResources

data NeutralSceneTarget

type instance (InvokeReq NeutralSceneTarget sreq) =
  (
    HasType "shader" FilePath sreq,
    HasType "geometry" (DataSource BasicDataSourceTypes) sreq,
    HasType "textures" [DataSource BasicDataSourceTypes] sreq
  )

type instance (DrawReq NeutralSceneTarget dreq) =
  (
    HasType "modelMatrix" (M44 Float) dreq,
    HasType "viewMatrix" (M44 Float) dreq,
    HasType "projectionMatrix" (M44 Float) dreq
  )


