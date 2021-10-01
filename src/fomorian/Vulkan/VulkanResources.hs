{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Fomorian.Vulkan.VulkanResources where

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
import Fomorian.Vulkan.DeviceMemoryTypes (AbstractMemoryType(..))
import Fomorian.Vulkan.DeviceMemoryAllocator


import Vulkan.Core10 (Buffer, Device, MemoryRequirements(..), BufferUsageFlags, BufferCreateInfo(..), DeviceSize)
import qualified Vulkan.Core10 as VK
import Vulkan.Zero as VZ

data MemoryBlockType = VertexMemory | IndexMemory | ImageMemory deriving (Eq, Show)

data VBuffer = VBuffer Buffer (MemoryAllocation DeviceSize) deriving (Eq, Show)
data IBuffer = IBuffer Buffer (MemoryAllocation DeviceSize) deriving (Eq, Show)

type VulkanDataSourceTypes = BasicDataSourceTypes
  -- | Allocator that manages vulkan CPU/GPU memory
  .+ ("placeholderSource" .== ())
  
type VulkanResourceTypes =
     ("vkGeometry" .==  GeometryResource VBuffer IBuffer String)
  .+ ("placeholderResource" .== Int)

data LoaderBundle =
  LoaderBundle {
    wBundle :: WindowBundle,
    memoryAllocator :: TMVar (MemoryAllocatorState AbstractMemoryType)
  }

computeVulkanResourceDependencies :: LoaderBundle -> DataSource VulkanDataSourceTypes -> IO [DataSource VulkanDataSourceTypes]
computeVulkanResourceDependencies _lb (DataSource d) = switch d $
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

loadVulkanResource :: LoaderBundle -> DataSource VulkanDataSourceTypes -> [Resource VulkanResourceTypes] -> IO (Resource VulkanResourceTypes)
loadVulkanResource lb (DataSource r) deps =
  let inMonad = runM . runLoaderMonad lb
  in
    case trial r #placeholderSource of
      Right () -> return $ Resource $ IsJust #placeholderResource 0
      Left baseSource -> do
        (Resource baseResult) <- loadBasicData (DataSource baseSource)
        switch baseResult $
             (#vertexPositions  .== inMonad . (loadVertexPositions deps))
          .+ (#vertexData       .== inMonad . (loadVertexData deps))
          .+ (#shaderBytes      .== undefined)
          .+ (#textureBytes     .== undefined)

unloadVulkanResource :: LoaderBundle -> DataSource VulkanDataSourceTypes -> Resource VulkanResourceTypes -> IO ()
unloadVulkanResource lb _ (Resource r) = 
  let inMonad = runM . runLoaderMonad lb
  in
    switch r $
         (#vkGeometry          .== inMonad . unloadGeometry)
      .+ (#placeholderResource .== noUnload)
        where
          noUnload _ = return ()

vulkanLoaderConfig :: LoaderBundle -> ResourceLoaderConfig (DataSource VulkanDataSourceTypes) (Resource VulkanResourceTypes)
vulkanLoaderConfig lb =
  ResourceLoaderConfig {
    loadIO = (\l deps -> loadVulkanResource lb l deps),
    unloadIO = (\l r -> unloadVulkanResource lb l r),
    dependenciesIO = computeVulkanResourceDependencies lb
  }

startLoader :: WindowBundle -> IO (LoaderBundle, ForkLoaderResult (LoaderRequest (DataSource VulkanDataSourceTypes) (Resource VulkanResourceTypes)) (LoaderResult (DataSource VulkanDataSourceTypes) (Resource VulkanResourceTypes)))
startLoader wb = do
  allocator <- startMemoryAllocator wb
  let lb = LoaderBundle wb allocator
  let cfg = vulkanLoaderConfig lb
  loader <- forkLoader 4 cfg
  return (lb, loader)
    
endLoader :: LoaderBundle -> ForkLoaderResult (LoaderRequest (DataSource VulkanDataSourceTypes) (Resource VulkanResourceTypes)) (LoaderResult (DataSource VulkanDataSourceTypes) (Resource VulkanResourceTypes)) -> IO ()
endLoader (LoaderBundle _ allocator) loader = do
  shutdownLoader loader
  endMemoryAllocator allocator

--
-- memory allocator
--

-- | Loader monad. Load/unlods run in this monad for easy access to the vulkan device and memory allocator
data VulkanLoader r where
  AllocateV :: MemoryRequirements -> AbstractMemoryType -> VulkanLoader (MemoryAllocation DeviceSize)
  DeallocateV :: MemoryAllocation DeviceSize -> VulkanLoader ()
  GetDevice :: VulkanLoader Device

type InLoaderMonad effs = (Member VulkanLoader effs, LastMember IO effs)

allocateV :: forall effs. Member VulkanLoader effs => MemoryRequirements -> AbstractMemoryType -> Eff effs (MemoryAllocation DeviceSize)
allocateV req memType = send (AllocateV req memType)

deallocateV :: forall effs. Member VulkanLoader effs => MemoryAllocation DeviceSize -> Eff effs ()
deallocateV allocation = send (DeallocateV allocation)

getDevice :: forall effs. Member VulkanLoader effs => Eff effs Device
getDevice = send GetDevice

runLoaderMonad :: forall effs. LastMember IO effs => LoaderBundle -> Eff (VulkanLoader ': effs) ~> Eff effs
runLoaderMonad lBundle = runReader lBundle . vulkanMemToState
  where
    vulkanMemToState :: forall effz. LastMember IO effz => Eff (VulkanLoader ': effz) ~> Eff (Reader LoaderBundle ': effz)
    vulkanMemToState = reinterpret $ \case
        AllocateV m t -> do
          lb <- ask
          result <- sendM $ allocateSTM (memoryAllocator lb) m t
          case result of
            Nothing -> error "argh"
            Just x -> return x
        DeallocateV b -> do
          lb <- ask
          sendM $ deallocateSTM (memoryAllocator lb) b
          return ()
        GetDevice -> do
          lb <- ask
          let wb = wBundle lb
          return $ Fomorian.Vulkan.WindowBundle.deviceHandle (vulkanDeviceBundle wb)



startMemoryAllocator :: WindowBundle -> IO (TMVar (MemoryAllocatorState AbstractMemoryType))
startMemoryAllocator wb = do
  let db = vulkanDeviceBundle wb
  let d = Fomorian.Vulkan.WindowBundle.deviceHandle db
  let pd = Fomorian.Vulkan.WindowBundle.physicalDeviceHandle db
  props <- VK.getPhysicalDeviceMemoryProperties pd
  --putStrLn $ "startMemory Allocator: " Prelude.++ show props
  let allocatorConfig = AllocatorConfig (genericChooseArenaSize props)
  allocator <- mkMemoryAllocator d pd allocatorConfig
  atomically $ newTMVar allocator

endMemoryAllocator :: TMVar (MemoryAllocatorState AbstractMemoryType) -> IO ()
endMemoryAllocator memMVar = do
  mem <- atomically $ takeTMVar memMVar
  cleanupMemoryAllocator mem


allocateSTM :: TMVar (MemoryAllocatorState AbstractMemoryType) -> MemoryRequirements -> AbstractMemoryType -> IO (Maybe (MemoryAllocation DeviceSize))
allocateSTM memVar (MemoryRequirements size alignment allowedTypeBits) memType = do
  -- we use take so that no one else can allocate until this function is done
  memState <- atomically (takeTMVar memVar)
  (alloc, memState') <- allocateDeviceMemory memState size (MemAlign alignment) memType allowedTypeBits
  atomically $ putTMVar memVar memState'
  return alloc
  
deallocateSTM :: TMVar (MemoryAllocatorState AbstractMemoryType) -> MemoryAllocation DeviceSize -> IO ()
deallocateSTM memVar memBlock = do
  memState <- atomically (takeTMVar memVar)
  freeResult <- freeDeviceMemory memState memBlock
  case freeResult of
    Nothing -> atomically $ putTMVar memVar memState
    Just memState' -> atomically $ putTMVar memVar memState'

--
-- geometry
--

loadVertexPositions :: InLoaderMonad effs => [Resource VulkanResourceTypes] -> GeometryResource [V3 Float] [Int] VertexAttribute -> Eff effs (Resource VulkanResourceTypes)
loadVertexPositions _deps geo = do
  geo' <- loadGeometry geo
  return $ Resource $ IsJust #vkGeometry geo'

loadVertexData :: [Resource VulkanResourceTypes] -> GeometryResource [Float] [Int] VertexAttribute -> Eff effs (Resource VulkanResourceTypes)
loadVertexData _deps _geo = do
  return $ Resource $ IsJust #vkGeometry undefined


loadGeometry :: (InLoaderMonad effs, Storable x) => GeometryResource [x] [Int] VertexAttribute -> Eff effs (GeometryResource VBuffer IBuffer String)
loadGeometry geo = do
  d <- getDevice
  (vbuf,vmem) <- loadStaticBuffer (vBuffer geo) VK.BUFFER_USAGE_VERTEX_BUFFER_BIT PreferGPU
  iBuffer <- case indexBuffer geo of
               Nothing -> return Nothing
               Just iValues -> do
                 (ibuf,imem) <- loadStaticBuffer iValues VK.BUFFER_USAGE_INDEX_BUFFER_BIT PreferGPU
                 return $ Just (IBuffer ibuf imem)
  let vertexBuffer = (VBuffer vbuf vmem)
  let elements = case indexBuffer geo of
                   Nothing -> (Prelude.length (vBuffer geo)) `div` 3
                   Just ib -> (Prelude.length ib)            `div` 3
  return (GeometryResource vertexBuffer iBuffer elements M.empty)

-- | Allocate a chunk of memory using the allocator and bind a buffer to it.
makeBuffer :: (InLoaderMonad effs) => DeviceSize -> BufferUsageFlags -> AbstractMemoryType -> Eff effs (Buffer, MemoryAllocation DeviceSize)
makeBuffer bSize bUsage memType = do
  d <- getDevice
  let bufInfo = BufferCreateInfo () VZ.zero bSize bUsage VK.SHARING_MODE_EXCLUSIVE V.empty
  buf <- VK.createBuffer d bufInfo Nothing
  req <- VK.getBufferMemoryRequirements d buf
  allocResult <- allocateV req memType
  let (MemoryAllocation memHandle _ _ block) = allocResult
  VK.bindBufferMemory d buf memHandle (blockOffset block)
  return $ (buf, allocResult)

-- | Allocate a new buffer and fill it with data 
-- right now the memory type is ignored and we always use 'RequireHostVisible'
-- should redo to allow any memory type and use a staging buffer for GPU-only accessible memory
--
-- Note this type definition has a 'forall' so that 'x' can be used via ScopedTypeVariables
loadStaticBuffer :: forall effs x. (InLoaderMonad effs, Storable x) => [x] -> BufferUsageFlags -> AbstractMemoryType -> Eff effs (Buffer, MemoryAllocation DeviceSize)
loadStaticBuffer arrayData usage memType = do
  d <- getDevice
  let size = fromIntegral $ sizeOf (undefined :: x) * Prelude.length arrayData
  (b,alloc) <- makeBuffer size usage RequireHostVisible
  let (MemoryAllocation memHandle _ _ block) = alloc
  sendM $ VK.withMappedMemory d memHandle (blockOffset block) (blockSize block) VZ.zero bracket $ \ptr -> pokeArray (castPtr ptr) arrayData
  return (b,alloc)


unloadGeometry :: (InLoaderMonad effs) => GeometryResource VBuffer IBuffer String -> Eff effs ()
unloadGeometry (GeometryResource vb ib _ _) = do
  destroyVBuffer vb
  mapM_ destroyIBuffer ib
  return ()

destroyVBuffer :: (InLoaderMonad effs) => VBuffer -> Eff effs ()
destroyVBuffer (VBuffer buf alloc) = do
  d <- getDevice
  VK.destroyBuffer d buf Nothing
  deallocateV alloc

destroyIBuffer :: (InLoaderMonad effs) => IBuffer -> Eff effs ()
destroyIBuffer (IBuffer buf alloc) = do
  d <- getDevice
  VK.destroyBuffer d buf Nothing
  deallocateV alloc


