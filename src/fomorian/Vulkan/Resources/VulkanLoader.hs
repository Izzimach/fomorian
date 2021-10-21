{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Module for the vulkan resource loader. This hooks up all the calls to load/unload various resources and memory.
module Fomorian.Vulkan.Resources.VulkanLoader where

import GHC.Generics

import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Concurrent
import Control.Concurrent.Async.Pool
import Control.Exception
import Control.Concurrent.STM

import Control.Monad.Freer
import Control.Monad.Freer.Reader (Reader(..), ask, runReader)

import Data.Maybe (isJust)
import Data.Word
import Data.Row
import Data.Row.Variants (view)
import Data.Functor.Foldable
import Data.Foldable (find)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Vector as V hiding (mapM_)

import Foreign.Storable (Storable, sizeOf)
import Foreign.Marshal.Array
import Foreign.Ptr (nullPtr, plusPtr,castPtr)
import System.FilePath

import Linear

import LoadUnload
import AsyncLoader

import Fomorian.SceneNode
import Fomorian.NeutralSceneTarget
import Fomorian.SceneResources
import Fomorian.SimpleMemoryArena
import Fomorian.GraphicsLoaders.ProcessWavefront (OBJBufferRecord, loadWavefrontOBJFile)

import Fomorian.Vulkan.WindowBundle
import Fomorian.Vulkan.VulkanMonads
import Fomorian.Vulkan.Resources.BoundCommandBuffer
import Fomorian.Vulkan.Resources.DeviceMemoryTypes (AbstractMemoryType(..))
import Fomorian.Vulkan.Resources.DeviceMemoryAllocator
import Fomorian.Vulkan.Resources.VulkanResourcesBase
import Fomorian.Vulkan.Resources.DataBuffers


import Vulkan.Core10 (Buffer, Device, MemoryRequirements(..), BufferUsageFlags, BufferCreateInfo(..), DeviceSize)
import qualified Vulkan.Core10 as VK
import Vulkan.Zero as VZ

computeVulkanResourceDependencies :: WindowBundle -> DataSource VulkanDataSourceTypes -> IO [DataSource VulkanDataSourceTypes]
computeVulkanResourceDependencies _wb (DataSource d) = switch d $
     (#coordinates2d    .== noDep)
  .+ (#coordinates3d    .== noDep)
  .+ (#rawVertexAttribs .== noDep)
  .+ (#wavefrontPath    .== noDep)
  .+ (#shaderPath       .== noDep)
  .+ (#texturePath      .== noDep)
  .+ (#placeholderSource .== noDep)
    where
      noDep :: a -> IO [DataSource VulkanDataSourceTypes]
      noDep _ = return []

loadVulkanResource :: WindowBundle -> BoundQueueThread -> DataSource VulkanDataSourceTypes -> [Resource VulkanResourceTypes] -> IO (Resource VulkanResourceTypes)
loadVulkanResource wb bQ (DataSource r) deps =
  let inMonad = runM . runVulkanMonad wb . runBoundOneShot bQ
  in
    case trial r #placeholderSource of
      Right () -> return $ Resource $ IsJust #placeholderResource 0
      Left baseSource -> do
        (Resource baseResult) <- loadBasicData (DataSource baseSource)
        switch baseResult $
             (#vertexPositions  .== inMonad . loadVertexPositions deps)
          .+ (#vertexData       .== inMonad . loadVertexData deps)
          .+ (#shaderBytes      .== undefined)
          .+ (#textureBytes     .== undefined)

unloadVulkanResource :: WindowBundle -> BoundQueueThread -> DataSource VulkanDataSourceTypes -> Resource VulkanResourceTypes -> IO ()
unloadVulkanResource wb bQ _ (Resource r) = 
  let inMonad = runM . runVulkanMonad wb . runBoundOneShot bQ
  in
    switch r $ 
         ((#vkGeometry          .== inMonad . unloadGeometry)
      .+ (#placeholderResource  .== noUnload)
      .+ (#textureImage         .== noUnload))
        where
          noUnload _ = return ()

vulkanLoaderConfig :: WindowBundle -> BoundQueueThread -> ResourceLoaderConfig (DataSource VulkanDataSourceTypes) (Resource VulkanResourceTypes)
vulkanLoaderConfig wb bQ =
  ResourceLoaderConfig {
    loadIO = loadVulkanResource wb bQ,
    unloadIO = unloadVulkanResource wb bQ,
    dependenciesIO = computeVulkanResourceDependencies wb
  }

startLoader :: WindowBundle -> BoundQueueThread -> IO (ForkLoaderResult (LoaderRequest (DataSource VulkanDataSourceTypes) (Resource VulkanResourceTypes)) (LoaderResult (DataSource VulkanDataSourceTypes) (Resource VulkanResourceTypes)))
startLoader wb bQ = do
  let cfg = vulkanLoaderConfig wb bQ
  forkLoader 4 cfg
    
endLoader :: ForkLoaderResult (LoaderRequest (DataSource VulkanDataSourceTypes) (Resource VulkanResourceTypes)) (LoaderResult (DataSource VulkanDataSourceTypes) (Resource VulkanResourceTypes)) -> IO ()
endLoader loader = do
  shutdownLoader loader


