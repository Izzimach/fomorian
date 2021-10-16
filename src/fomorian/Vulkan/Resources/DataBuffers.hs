{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


-- | Module to handle buffers: vertex buffers, index buffers, uniform buffers. Includes convenience functions to allocate
---  memory and bind it, and also staging buffers used to transfer data to device local memory
module Fomorian.Vulkan.Resources.DataBuffers where

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
import Fomorian.Vulkan.VulkanMonads
import Fomorian.Vulkan.Resources.DeviceMemoryTypes (AbstractMemoryType(..))
import Fomorian.Vulkan.Resources.DeviceMemoryAllocator
import Fomorian.Vulkan.Resources.VulkanResourcesBase
import qualified Vulkan.Core12 as VK


--
-- geometry
--

loadVertexPositions :: (InVulkanMonad effs) => [Resource VulkanResourceTypes] -> GeometryResource [V3 Float] [Int] VertexAttribute -> Eff effs (Resource VulkanResourceTypes)
loadVertexPositions _deps geo = do
  geo' <- loadGeometry geo
  return $ Resource $ IsJust #vkGeometry geo'

loadVertexData :: [Resource VulkanResourceTypes] -> GeometryResource [Float] [Int] VertexAttribute -> Eff effs (Resource VulkanResourceTypes)
loadVertexData _deps _geo = do
  return $ Resource $ IsJust #vkGeometry undefined


loadGeometry :: (InVulkanMonad effs, Storable x) => GeometryResource [x] [Int] VertexAttribute -> Eff effs (GeometryResource VBuffer IBuffer String)
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





makeUniformBuffer :: (InVulkanMonad effs) => DeviceSize -> Eff effs UBuffer
makeUniformBuffer bufferSize = do
  (b,alloc) <- makeBuffer bufferSize VK.BUFFER_USAGE_UNIFORM_BUFFER_BIT RequireHostVisible
  return (UBuffer b alloc)


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

-- | Allocate a new buffer and fill it with data. The data is just an array of storable values, so it can be used for vertices, indices, etc.
--
-- right now the memory type is ignored and we always use 'RequireHostVisible'
-- should redo to allow any memory type and use a staging buffer for GPU-only accessible memory
--
-- Note this type definition has a 'forall' so that 'x' can be used via ScopedTypeVariables
loadStaticBuffer :: forall effs x. (InVulkanMonad effs, Storable x) => [x] -> BufferUsageFlags -> AbstractMemoryType -> Eff effs (Buffer, MemoryAllocation DeviceSize)
loadStaticBuffer arrayData usage memType = do
  d <- getDevice
  let size = fromIntegral $ sizeOf (undefined :: x) * Prelude.length arrayData
  (b,alloc) <- makeBuffer size usage RequireHostVisible
  let (MemoryAllocation memHandle _ _ block) = alloc
  sendM $ VK.withMappedMemory d memHandle (blockOffset block) (blockSize block) VZ.zero bracket $ \ptr -> pokeArray (castPtr ptr) arrayData
  return (b,alloc)


unloadGeometry :: (InVulkanMonad effs) => GeometryResource VBuffer IBuffer String -> Eff effs ()
unloadGeometry (GeometryResource vb ib _ _) = do
  destroyVBuffer vb
  mapM_ destroyIBuffer ib
  

destroyVBuffer :: (InVulkanMonad effs) => VBuffer -> Eff effs ()
destroyVBuffer (VBuffer buf alloc) = do
  d <- getDevice
  VK.destroyBuffer d buf Nothing
  deallocateV alloc

destroyIBuffer :: (InVulkanMonad effs) => IBuffer -> Eff effs ()
destroyIBuffer (IBuffer buf alloc) = do
  d <- getDevice
  VK.destroyBuffer d buf Nothing
  deallocateV alloc

destroyUBuffer :: (InVulkanMonad effs) => UBuffer -> Eff effs ()
destroyUBuffer (UBuffer buf alloc) = do
  d <- getDevice
  VK.destroyBuffer d buf Nothing
  deallocateV alloc
