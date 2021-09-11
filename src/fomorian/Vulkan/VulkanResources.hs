{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Fomorian.Vulkan.VulkanResources where

import GHC.Generics

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async.Pool

import Data.Maybe (isJust)
import Data.Row
import Data.Row.Variants (view)
import Data.Functor.Foldable
import Data.Foldable (find)
import qualified Data.Set as S
import qualified Data.Map as M

import Foreign.Storable (Storable, sizeOf)
import Foreign.Ptr (nullPtr, plusPtr)
import System.FilePath

import Linear

import LoadUnload

import Fomorian.SceneNode
import Fomorian.NeutralSceneTarget
import Fomorian.SceneResources
import Fomorian.GraphicsLoaders.ProcessWavefront (OBJBufferRecord, loadWavefrontOBJFile)


type VulkanDataSourceTypes = BasicDataSourceTypes
  -- | buffer accessible in CPU memory
  .+ ("mappedBuffer"     .== ())
  -- | buffer on GPU, can't be accessed by the CPU
  .+ ("gpuBuffer" .== ())
  -- | One of the pools from which you can allocate memory chunks for buffers/images
  .+ ("memoryPool" .== ())
  
type VulkanResourceTypes =
     ("vertexBuffer" .==  GeometryResource Int [Int] String)
