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

import Data.Maybe (isJust, fromMaybe)
import Data.Text (Text)
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

computeVulkanResourceDependencies :: WindowBundle -> VulkanDataSource -> IO (Set (VulkanDataSource))
computeVulkanResourceDependencies _wb (DataSource d) = switch d $
     (#placeholderSource .== noDep)
  .+ (#userSource        .== noDep)
  .+ (#wavefrontPath     .== noDep)
  .+ (#shaderPath        .== noDep)
  .+ (#texturePath       .== noDep)
    where
      noDep _ = return S.empty

loadVulkanResource :: WindowBundle -> BoundQueueThread -> Map Text BasicResource -> VulkanDataSource -> Map VulkanDataSource VulkanResource -> IO VulkanResource
loadVulkanResource wb bQ prebuilt (DataSource r) depsMap = switch r $
     #placeholderSource .== (\_ -> return (Resource (IsJust #placeholderResource 0)))
  .+ #userSource        .== (\l -> basicVulkanLoad (fromMaybe undefined (M.lookup l prebuilt)))
  .+ #wavefrontPath     .== (\fp -> do g <- wavefrontGeometry fp; basicVulkanLoad (Resource $ IsJust #vertexFloats g))
  .+ #shaderPath        .== loadVulkanShaderFromPath
  .+ #texturePath       .== loadVulkanTextureFromPath
  where
    inMonad = runM . runVulkanMonad wb . runBoundOneShot bQ

    loadVulkanTextureFromPath tp =  do
      t <- inMonad $ makeTextureImage ("resources" </> "textures" </> tp) Nothing
      return $ Resource $ IsJust #textureImage t

    loadVulkanShaderFromPath = undefined

    basicVulkanLoad :: BasicResource -> IO VulkanResource
    basicVulkanLoad (Resource baseResource) = 
      let inMonad = runM . runVulkanMonad wb . runBoundOneShot bQ
      in switch baseResource $
                 (#vertexPositions  .== inMonad . loadVertexPositions depsMap)
              .+ (#vertexFloats     .== inMonad . loadVertexData depsMap)
              .+ (#vertexFunction   .== undefined)
              .+ (#shaderBytes      .== undefined)
              .+ (#textureBytes     .== undefined )

unloadVulkanResource :: WindowBundle -> BoundQueueThread -> ResourceInfo VulkanDataSource VulkanResource -> IO ()
unloadVulkanResource wb bQ (ResourceInfo _ (Resource r) _) = 
  let inMonad = runM . runVulkanMonad wb . runBoundOneShot bQ
  in
    switch r 
         ((#vkGeometry          .== inMonad . unloadGeometry)
      .+ (#placeholderResource  .== noUnload)
      .+ (#textureImage         .== inMonad . unmakeTextureImage ))
        where
          noUnload _ = return ()

vulkanLoaderCallbacks :: WindowBundle -> BoundQueueThread -> Map Text BasicResource -> LoadUnloadCallbacks VulkanDataSource VulkanResource VulkanError
vulkanLoaderCallbacks wb bQ prebuilt =
  LoadUnloadCallbacks {
    loadResource = loadVulkanResource wb bQ prebuilt,
    unloadResource = unloadVulkanResource wb bQ,
    findDependencies = computeVulkanResourceDependencies wb,
    processException = const Drop
  }

startLoader :: WindowBundle -> BoundQueueThread -> Map Text BasicResource -> IO (AsyncLoader VulkanDataSource VulkanResource VulkanError)
startLoader wb bQ prebuilt = do
  let asyncConfig = AsyncLoaderConfig 2 simpleThreadWrapper simpleThreadWrapper
  let callbacks = vulkanLoaderCallbacks wb bQ prebuilt
  startAsyncLoader asyncConfig callbacks
    
endLoader :: AsyncLoader (VulkanDataSource) (VulkanResource) VulkanError -> IO ()
endLoader loader = do
  shutdownAsyncLoader loader


