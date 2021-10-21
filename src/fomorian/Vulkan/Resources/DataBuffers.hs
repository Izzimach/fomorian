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
import Data.Row
import qualified Data.Map as M
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
import Fomorian.SimpleMemoryArena
import Fomorian.Vulkan.WindowBundle
import Fomorian.Vulkan.VulkanMonads
import Fomorian.Vulkan.Resources.DeviceMemoryTypes (AbstractMemoryType(..))
import Fomorian.Vulkan.Resources.DeviceMemoryAllocator
import Fomorian.Vulkan.Resources.VulkanResourcesBase


--
-- geometry
--

loadVertexPositions :: (InVulkanMonad effs, Member OneShotSubmitter effs) => [Resource VulkanResourceTypes] -> GeometryResource [V3 Float] [Int] VertexAttribute -> Eff effs (Resource VulkanResourceTypes)
loadVertexPositions _deps geo = do
  geo' <- loadGeometry geo
  return $ Resource $ IsJust #vkGeometry geo'

loadVertexData :: [Resource VulkanResourceTypes] -> GeometryResource [Float] [Int] VertexAttribute -> Eff effs (Resource VulkanResourceTypes)
loadVertexData _deps _geo = do
  return $ Resource $ IsJust #vkGeometry undefined


loadGeometry :: (InVulkanMonad effs, Storable x, Member OneShotSubmitter effs) => GeometryResource [x] [Int] VertexAttribute -> Eff effs (GeometryResource VBuffer IxBuffer VertexAttribute)
loadGeometry geo = do
  d <- getDevice
  (vbuf,vmem) <- loadStaticBuffer (vBuffer geo) VK.BUFFER_USAGE_VERTEX_BUFFER_BIT PreferGPU
  ixBuffer <- case indexBuffer geo of
               Nothing -> return Nothing
               Just iValues -> do
                 (ibuf,imem) <- loadStaticBuffer iValues VK.BUFFER_USAGE_INDEX_BUFFER_BIT PreferGPU
                 return $ Just (IxBuffer ibuf imem)
  let vertexBuffer = VBuffer vbuf vmem
  let elements = case indexBuffer geo of
                   Nothing -> Prelude.length (vBuffer geo) `div` 3
                   Just ib -> Prelude.length ib            `div` 3
  return (GeometryResource vertexBuffer ixBuffer elements (attributeMap geo))


-- | Makes a single uniform buffer. These are always host visible since you'll probably dump stuff in there a lot.
makeUniformBuffer :: (InVulkanMonad effs) => DeviceSize -> Eff effs UBuffer
makeUniformBuffer bufferSize = do
  (b,alloc) <- makeBuffer bufferSize VK.BUFFER_USAGE_UNIFORM_BUFFER_BIT RequireHostVisible
  return (UBuffer b alloc)

-- | Create a CPU-accessable buffer, usually used just for copying over to the GPU.
loadStagingBuffer :: forall effs v. (InVulkanMonad effs, Storable v) => [v] -> BufferUsageFlags -> Eff effs StagingBuffer
loadStagingBuffer arrayData usage = do
  d <- getDevice
  let size = fromIntegral $ sizeOf (undefined :: v) * Prelude.length arrayData
  (b,alloc) <- makeBuffer size (usage .|. VK.BUFFER_USAGE_TRANSFER_SRC_BIT) RequireHostVisible
  let (MemoryAllocation memHandle _ _ block) = alloc
  sendM $ VK.withMappedMemory d memHandle (blockOffset block) (blockSize block) VZ.zero bracket $ \ptr -> pokeArray (castPtr ptr) arrayData
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
  let (MemoryAllocation memHandle memTypeIndex _ block) = alloc
  memManagerVar <- memoryManager <$> getWindowBundle
  memManager <- sendM $ atomically $ readTMVar memManagerVar
  -- if the buffer is host visible we can just map it and copy over the data. if not, we need to put the data into a staging buffer
  -- and transfer that data to the buffer via a command buffer operation
  if isHostVisible memManager alloc
  then sendM $ VK.withMappedMemory d memHandle (blockOffset block) (blockSize block) VZ.zero bracket $ \ptr -> pokeArray (castPtr ptr) arrayData
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
  let (MemoryAllocation memHandle _ _ block) = allocResult
  VK.bindBufferMemory d buf memHandle (blockOffset block)
  return (buf, allocResult)


unloadGeometry :: (InVulkanMonad effs) => GeometryResource VBuffer IxBuffer VertexAttribute -> Eff effs ()
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
