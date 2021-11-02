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

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Vector as V hiding (mapM_)

import Foreign.Storable (Storable, sizeOf)
import Foreign.Marshal.Array
import Foreign.Ptr (nullPtr, plusPtr,castPtr)

import System.FilePath

import Linear

import STMLoader.LoadUnload
import STMLoader.AsyncLoader

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
import Fomorian.Vulkan.Resources.ImageBuffers (makeTextureImage, unmakeTextureImage)


import Vulkan.Core10 (Buffer, Device, MemoryRequirements(..), BufferUsageFlags, BufferCreateInfo(..), DeviceSize)
import qualified Vulkan.Core10 as VK
import Vulkan.Zero as VZ

type VulkanError = String

computeVulkanResourceDependencies :: WindowBundle -> DataSource VulkanDataSourceTypes -> IO (Set (DataSource VulkanDataSourceTypes))
computeVulkanResourceDependencies _wb (DataSource d) = switch d $
     (#coordinates2d    .== noDep)
  .+ (#coordinates3d    .== noDep)
  .+ (#rawVertexAttribs .== noDep)
  .+ (#wavefrontPath    .== noDep)
  .+ (#shaderPath       .== noDep)
  .+ (#texturePath      .== noDep)
  .+ (#placeholderSource .== noDep)
    where
      noDep _ = return S.empty

loadVulkanResource :: WindowBundle -> BoundQueueThread -> DataSource VulkanDataSourceTypes -> Map (DataSource VulkanDataSourceTypes) (Resource VulkanResourceTypes) -> IO (Resource VulkanResourceTypes)
loadVulkanResource wb bQ (DataSource r) depsMap =
  let inMonad = runM . runVulkanMonad wb . runBoundOneShot bQ
  in
    case trial r #placeholderSource of
      Right () -> return $ Resource $ IsJust #placeholderResource 0
      Left baseSource -> 
        case trial baseSource #texturePath of
          Right tp -> do t <- inMonad $ makeTextureImage ("resources" </> "textures" </> tp) Nothing
                         return $ Resource $ IsJust #textureImage t
          Left _ -> basicVulkanLoad wb bQ (DataSource baseSource)
    where
      basicVulkanLoad :: WindowBundle -> BoundQueueThread -> DataSource BasicDataSourceTypes -> IO (Resource VulkanResourceTypes)
      basicVulkanLoad wb gQ baseSource = 
        let inMonad = runM . runVulkanMonad wb . runBoundOneShot gQ
        in do
              (Resource baseResult) <- loadBasicData baseSource
              switch baseResult $
                   (#vertexPositions  .== inMonad . loadVertexPositions depsMap)
                .+ (#vertexData       .== inMonad . loadVertexData depsMap)
                .+ (#shaderBytes      .== undefined)
                .+ (#textureBytes     .== undefined )

        

unloadVulkanResource :: WindowBundle -> BoundQueueThread -> ResourceInfo (DataSource VulkanDataSourceTypes) (Resource VulkanResourceTypes) -> IO ()
unloadVulkanResource wb bQ (ResourceInfo _ (Resource r) _) = 
  let inMonad = runM . runVulkanMonad wb . runBoundOneShot bQ
  in
    switch r 
         ((#vkGeometry          .== inMonad . unloadGeometry)
      .+ (#placeholderResource  .== noUnload)
      .+ (#textureImage         .== inMonad . unmakeTextureImage ))
        where
          noUnload _ = return ()

vulkanLoaderCallbacks :: WindowBundle -> BoundQueueThread -> LoadUnloadCallbacks (DataSource VulkanDataSourceTypes) (Resource VulkanResourceTypes) VulkanError
vulkanLoaderCallbacks wb bQ =
  LoadUnloadCallbacks {
    loadResource = loadVulkanResource wb bQ,
    unloadResource = unloadVulkanResource wb bQ,
    findDependencies = computeVulkanResourceDependencies wb,
    processException = const Drop
  }

startLoader :: WindowBundle -> BoundQueueThread -> IO (AsyncLoader (DataSource VulkanDataSourceTypes) (Resource VulkanResourceTypes) VulkanError)
startLoader wb bQ = do
  let asyncConfig = AsyncLoaderConfig 2 simpleThreadWrapper simpleThreadWrapper
  let callbacks = vulkanLoaderCallbacks wb bQ
  startAsyncLoader asyncConfig callbacks
    
endLoader :: AsyncLoader (DataSource VulkanDataSourceTypes) (Resource VulkanResourceTypes) VulkanError -> IO ()
endLoader loader = do
  shutdownAsyncLoader loader


