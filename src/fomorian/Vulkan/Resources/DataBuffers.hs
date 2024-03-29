{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


-- | Module to handle buffers: vertex buffers, index buffers, uniform buffers. Includes convenience functions to allocate
---  memory and bind it, and also staging buffers used to transfer data to device local memory
module Fomorian.Vulkan.Resources.DataBuffers where

import Control.Concurrent.STM (atomically, readTMVar)

import Data.Bits
import Data.Word (Word32)
import Data.Row
import Data.Map (Map)
import qualified Data.Map as M
import Data.Vector (Vector)
import qualified Data.Vector as V

import Linear

import Control.Exception
import Control.Monad.Freer as CMF

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array

import Vulkan.Core10 (Buffer, BufferUsageFlags, BufferCreateInfo(..), DeviceSize)
import qualified Vulkan.Core10 as VK
import Vulkan.Zero as VZ

import Fomorian.SceneResources
import Fomorian.StorableLayout
import Fomorian.SimpleMemoryArena
import Fomorian.Vulkan.WindowBundle
import Fomorian.Vulkan.VulkanMonads
import Fomorian.Vulkan.Resources.DeviceMemoryTypes (AbstractMemoryType(..))
import Fomorian.Vulkan.Resources.DeviceMemoryAllocator
import Fomorian.Vulkan.Resources.VulkanResourcesBase


--
-- geometry
--

loadVertexPositions :: (InVulkanMonad effs, Member OneShotSubmitter effs) => Map VulkanDataSource VulkanResource -> GeometryResource [V3 Float] [Int] DataLayoutMap -> Eff effs VulkanResource
loadVertexPositions _deps geo = do
  geo' <- loadGeometry geo
  return $ Resource $ IsJust #vkGeometry geo'

loadVertexData :: (InVulkanMonad effs, Member OneShotSubmitter effs) => Map VulkanDataSource VulkanResource -> GeometryResource [Float] [Int] DataLayoutMap -> Eff effs VulkanResource
loadVertexData _deps geo = do
  geo' <- loadGeometry geo
  return $ Resource $ IsJust #vkGeometry geo'


loadGeometry :: (InVulkanMonad effs, Storable x, Member OneShotSubmitter effs) => GeometryResource [x] [Int] DataLayoutMap -> Eff effs (GeometryResource VBuffer IxBuffer DataLayoutMap)
loadGeometry geo = do
  d <- getDevice
  (vbuf,vmem) <- loadStaticBuffer (vBuffer geo) VK.BUFFER_USAGE_VERTEX_BUFFER_BIT RequireHostVisible --PreferGPU
  ixBuffer <- case indexBuffer geo of
               Nothing -> return Nothing
               Just iValues -> do
                 (ibuf,imem) <- loadStaticBuffer (fmap toW32 iValues) VK.BUFFER_USAGE_INDEX_BUFFER_BIT RequireHostVisible --PreferGPU
                 return $ Just (IxBuffer ibuf imem)
  let vertexBuffer = VBuffer vbuf vmem
  let elements = case indexBuffer geo of
                   Nothing -> Prelude.length (vBuffer geo)
                   Just ib -> Prelude.length ib
  return (GeometryResource vertexBuffer ixBuffer elements (attributeMap geo))
    where
      toW32 :: Int -> Word32
      toW32 = fromIntegral


-- | Makes a single uniform buffer. These are always mapped since you'll probably dump stuff in there a lot.
makeUniformBuffer :: (InVulkanMonad effs) => DeviceSize -> Eff effs UBuffer
makeUniformBuffer bufferSize = do
  (b,alloc) <- makeBuffer bufferSize VK.BUFFER_USAGE_UNIFORM_BUFFER_BIT AlwaysMapped
  return (UBuffer b alloc)

-- | Create a CPU-accessable buffer, usually used just for copying over to the GPU.
loadStagingBuffer :: forall effs v. (InVulkanMonad effs, Storable v) => [v] -> BufferUsageFlags -> Eff effs StagingBuffer
loadStagingBuffer arrayData usage = do
  d <- getDevice
  let size = fromIntegral $ sizeOf (undefined :: v) * Prelude.length arrayData
  (b,alloc) <- makeBuffer size (usage .|. VK.BUFFER_USAGE_TRANSFER_SRC_BIT) AlwaysMapped
  let (MemoryAllocation memHandle _ _ memPtr block) = alloc
  case memPtr of
    Nothing -> error "Staging buffer was not automapped"
    Just ptr -> sendM $ pokeArray (castPtr ptr) arrayData
  return (StagingBuffer b alloc)

-- | Allocate a new buffer and fill it with data. The data is just an array of storable values, so it can be used for vertices, indices, etc.
--
--  Note this type definition has a 'forall' so that 'x' can be used via ScopedTypeVariables
loadStaticBuffer :: forall effs x. (InVulkanMonad effs, Member OneShotSubmitter effs, Storable x) => [x] -> BufferUsageFlags -> AbstractMemoryType -> Eff effs (Buffer, MemoryAllocation DeviceSize)
loadStaticBuffer arrayData usage memType = do
  d <- getDevice
  let size = fromIntegral $ sizeOf (undefined :: x) * Prelude.length arrayData
  -- always make sure we can run a transfer command to this buffer, which we'll need if it's on device local memory
  (b,alloc) <- makeBuffer size (usage .|. VK.BUFFER_USAGE_TRANSFER_DST_BIT) memType
  let (MemoryAllocation memHandle memTypeIndex _ memPtr block) = alloc
  memManagerVar <- memoryManager <$> getWindowBundle
  memManager <- sendM $ atomically $ readTMVar memManagerVar
  -- if the buffer is host visible we can just map it and copy over the data. if not, we need to put the data into a staging buffer
  -- and transfer that data to the buffer via a command buffer operation
  if isHostVisible memManager alloc
  then case memPtr of
         Nothing -> sendM $ VK.withMappedMemory d memHandle (blockOffset block) (blockSize block) VZ.zero bracket $ \ptr -> pokeArray (castPtr ptr) arrayData
         Just ptr -> sendM $ pokeArray (castPtr ptr) arrayData
  else do
    staging@(StagingBuffer srcBuf srcAlloc) <- loadStagingBuffer arrayData usage
    oneShotCommand $ \cBuf -> VK.cmdCopyBuffer cBuf srcBuf b (V.fromList [VK.BufferCopy 0 0 size])
    destroyStagingBuffer staging
  return (b,alloc)


-- | Allocate a chunk of memory using the allocator and bind a buffer to it.
makeBuffer :: (InVulkanMonad effs) => DeviceSize -> BufferUsageFlags -> AbstractMemoryType -> Eff effs (Buffer, MemoryAllocation DeviceSize)
makeBuffer bSize bUsage memType = do
  d <- getDevice
  let bufInfo = BufferCreateInfo () VZ.zero bSize bUsage VK.SHARING_MODE_EXCLUSIVE V.empty
  buf <- VK.createBuffer d bufInfo Nothing
  req <- VK.getBufferMemoryRequirements d buf
  allocResult <- allocateV req memType
  let (MemoryAllocation memHandle _ _ _ block) = allocResult
  VK.bindBufferMemory d buf memHandle (blockOffset block)
  return (buf, allocResult)


unloadGeometry :: (InVulkanMonad effs) => GeometryResource VBuffer IxBuffer DataLayoutMap -> Eff effs ()
unloadGeometry (GeometryResource vb ib _ _) = do
  destroyVBuffer vb
  mapM_ destroyIxBuffer ib
  

destroyVBuffer :: (InVulkanMonad effs) => VBuffer -> Eff effs ()
destroyVBuffer (VBuffer buf alloc) = do
  d <- getDevice
  VK.destroyBuffer d buf Nothing
  deallocateV alloc

destroyIxBuffer :: (InVulkanMonad effs) => IxBuffer -> Eff effs ()
destroyIxBuffer (IxBuffer buf alloc) = do
  d <- getDevice
  VK.destroyBuffer d buf Nothing
  deallocateV alloc

destroyUBuffer :: (InVulkanMonad effs) => UBuffer -> Eff effs ()
destroyUBuffer (UBuffer buf alloc) = do
  d <- getDevice
  VK.destroyBuffer d buf Nothing
  deallocateV alloc

destroyStagingBuffer :: (InVulkanMonad effs) => StagingBuffer -> Eff effs ()
destroyStagingBuffer (StagingBuffer buf alloc) = do
  d <- getDevice
  VK.destroyBuffer d buf Nothing
  deallocateV alloc
