{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
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

import Data.Maybe (isJust, fromMaybe, fromJust)
import Data.Text (Text)
import Data.ByteString (readFile)
import Data.Word
import Data.Row
import Data.Row.Variants (view)
import Data.Functor.Foldable
import Data.Foldable (find)
import Data.Monoid

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Vector as V hiding (mapM_, (++))

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
import Fomorian.Vulkan.Resources.DescriptorSets
import Fomorian.Vulkan.Resources.DataBuffers
import Fomorian.Vulkan.Resources.Pipeline
import Fomorian.Vulkan.Resources.ImageBuffers (makeTextureImage, unmakeTextureImage)


import Vulkan.Core10 (Buffer, Device, MemoryRequirements(..), BufferUsageFlags, BufferCreateInfo(..), DeviceSize)
import qualified Vulkan.Core10 as VK
import Vulkan.Zero as VZ

type VulkanError = String

computeVulkanResourceDependencies :: WindowBundle -> VulkanDataSource -> IO (Set VulkanDataSource)
computeVulkanResourceDependencies _wb (DataSource d) = switch d $
     #renderPassFormat  .== noDep
  .+ #userSource        .== noDep
  .+ #wavefrontPath     .== noDep
  .+ #shaderPath        .== noDep
  .+ #texturePath       .== noDep
  .+ #pipelineSettings  .== (\(SimplePipelineSettings rpFormat shSource dsLayouts) ->
                              let singletonDeps = 
                                      [
                                        DataSource $ IsJust #renderPassFormat rpFormat,
                                        DataSource $ IsJust #shaderPath (shSource ++ "vert.spv"),
                                        DataSource $ IsJust #shaderPath (shSource ++ "frag.spv")
                                      ]
                                  dsDeps = fmap (DataSource . IsJust #descriptorLayoutInfo) dsLayouts
                              in return $ S.fromList (singletonDeps ++ dsDeps))
  .+ #descriptorLayoutInfo .== noDep
  .+ #descriptorSourceSettings  .== (\s -> return $ S.singleton (DataSource $ IsJust #descriptorLayoutInfo s))    -- ^ need a DescriptorSetLayout first
    where
      noDep _ = return S.empty  

findDep :: (KnownSymbol l) => Map VulkanDataSource VulkanResource -> Label l -> Maybe (VulkanResourceTypes .! l)
findDep deps label = getAlt $ M.foldr labelMatch (Alt Nothing) deps
  where
    labelMatch (Resource b) x = case trial b label of
                       Left _ -> x
                       Right v -> Alt (Just v) <> x


loadVulkanResource :: WindowBundle -> BoundQueueThread -> Map Text BasicResource -> VulkanDataSource -> Map VulkanDataSource VulkanResource -> IO VulkanResource
loadVulkanResource wb bQ prebuilt (DataSource r) depsMap = switch r $
     #userSource        .== (\l -> basicVulkanLoad (fromMaybe undefined (M.lookup l prebuilt)))
  .+ #wavefrontPath     .== (\fp -> do g <- wavefrontGeometry fp; basicVulkanLoad (Resource $ IsJust #vertexFloats g))
  .+ #shaderPath        .== loadVulkanShaderFromPath
  .+ #texturePath       .== loadVulkanTextureFromPath
  .+ #renderPassFormat  .== loadRenderPass
  .+ #pipelineSettings  .== undefined
  .+ #descriptorLayoutInfo .== inMonad . loadDescriptorSetLayout
  .+ #descriptorSourceSettings .== inMonad . loadDescriptorSetSource (findDep depsMap (Label @"descriptorSetLayout"))
  where
    -- we need this type declaration to avoid the "monomorphism restriction"
    inMonad :: Eff '[OneShotSubmitter, VulkanMonad, IO] x -> IO x
    inMonad = runM . runVulkanMonad wb . runBoundOneShot bQ

    loadRenderPass :: (VK.Format, VK.Format) -> IO VulkanResource
    loadRenderPass (cFormat,dFormat) = inMonad (Resource . IsJust #renderPass <$> makeSimpleRenderPass cFormat dFormat)

    loadDescriptorSetSource :: (InVulkanMonad effs) => Maybe VK.DescriptorSetLayout -> DescriptorSetInfo -> Eff effs VulkanResource
    loadDescriptorSetSource dLayout dInfo = Resource . IsJust #descriptorSetSource <$> makeDescriptorSetSource dInfo (fromJust dLayout)

    loadVulkanTextureFromPath :: FilePath -> IO VulkanResource
    loadVulkanTextureFromPath tp =  do
      t <- inMonad $ makeTextureImage ("resources" </> "textures" </> tp) Nothing
      return $ Resource $ IsJust #textureImage t

    loadVulkanShaderFromPath :: FilePath -> IO VulkanResource
    loadVulkanShaderFromPath sp = inMonad $ do
      sBytes <- sendM $ Data.ByteString.readFile sp
      d <- getDevice
      sx <- VK.createShaderModule d (VK.ShaderModuleCreateInfo () VZ.zero sBytes) Nothing
      return $ Resource $ IsJust #shaderModule sx

    basicVulkanLoad :: BasicResource -> IO VulkanResource
    basicVulkanLoad (Resource baseResource) = 
      switch baseResource $
                 (#vertexPositions  .== inMonad . loadVertexPositions depsMap)
              .+ (#vertexFloats     .== inMonad . loadVertexData depsMap)
              .+ (#vertexFunction   .== undefined)
              .+ (#shaderBytes      .== undefined)
              .+ (#textureBytes     .== undefined )

unloadVulkanResource :: WindowBundle -> BoundQueueThread -> ResourceInfo VulkanDataSource VulkanResource -> IO ()
unloadVulkanResource wb bQ (ResourceInfo _ (Resource r) _) = 
  let inMonad = runM . runVulkanMonad wb . runBoundOneShot bQ
      noUnload _ = return ()
  in
    switch r $
         #vkGeometry           .== inMonad . unloadGeometry
      .+ #textureImage         .== inMonad . unmakeTextureImage
      .+ #shaderModule         .== (\s -> inMonad $ do d <- getDevice; VK.destroyShaderModule d s Nothing)
      .+ #renderPass           .== (\rp -> inMonad $ do d <- getDevice; VK.destroyRenderPass d rp Nothing)
      .+ #simplePipeline       .== undefined
      .+ #descriptorSetLayout  .== inMonad . unloadDescriptorSetLayout
      .+ #descriptorSetSource  .== inMonad . destroyDescriptorSetSource

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


