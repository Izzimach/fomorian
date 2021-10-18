{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Fomorian.Vulkan.Resources.DescriptorSets where

import Control.Monad.Freer
import Control.Exception (bracket)

import Data.Vector (Vector(..), empty, fromList)

import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

import Linear (V2 (..), V3 (..), V4(..), M44, identity, (!*!), mkTransformation, mkTransformationMat, Quaternion, axisAngle, lookAt, perspective, ortho, transpose)

import Vulkan.Core10 (Device, Extent2D(..), DescriptorSet, DescriptorSetLayout, DescriptorPool, AllocationCallbacks)
import qualified Vulkan.Core10 as VK
import Vulkan.CStruct.Extends
import Vulkan.Core10.MemoryManagement as VKMEM
import Vulkan.Extensions.VK_KHR_swapchain as VKSWAPCHAIN
import Vulkan.Zero as VZ

import Fomorian.Vulkan.VulkanMonads
import Fomorian.Vulkan.WindowBundle
import Fomorian.Vulkan.Resources.DeviceMemoryAllocator
import Fomorian.SimpleMemoryArena
import Fomorian.Vulkan.Resources.VulkanResourcesBase


data UniformBufferObject = UBO {
  uboModel :: M44 Float,
  uboView  :: M44 Float,
  uboProjection :: M44 Float
  }
  deriving (Eq,Show)

instance Storable UniformBufferObject where
  sizeOf _ = sizeOf @(M44 Float) undefined + sizeOf @(M44 Float) undefined + sizeOf @(M44 Float) undefined
  alignment _ = 16 -- maybe only needs to be 4?
  peek p = do
    -- treat as an array of @M44 Float@ objects
    let pMats = castPtr @UniformBufferObject @(M44 Float) p
    m <- peekElemOff pMats 0
    v <- peekElemOff pMats 1
    p <- peekElemOff pMats 2
    return (UBO m v p)
  poke p (UBO m v prj) = do
    let pMats = castPtr @UniformBufferObject @(M44 Float) p
    pokeElemOff pMats 0 m
    pokeElemOff pMats 1 v
    pokeElemOff pMats 2 prj


-- | Perspective that projects z to the range [0,1] used by Vulkan.  We can't use 'Linear.Projection.perspective' since it projects z to [-1,1]
zeroOnePerspective :: Floating a =>  a -> a -> a -> a -> M44 a
zeroOnePerspective fov aspect near far =
  V4 (V4 x 0 0 0)
     (V4 0 y 0 0)
     (V4 0 0 z w)
     (V4 0 0 m 0)
  where
    tanHalfFov = tan $ fov / 2
    x = 1 / (aspect * tanHalfFov)
    y = 1 / tanHalfFov
    fpn = far + near
    fmn = far - near
    oon = 1 / near
    oof = 1 / far
    z = - far / fmn
    w = 1 / (oof-oon)
    m = (-1)

updateUBO :: (InVulkanMonad effs) => UBuffer -> Float -> Extent2D -> Eff effs ()
updateUBO ub elapsedTime (Extent2D width height) = do
  -- our shaders use premultiply so matrices need to be transposed
  let modelMatrix = transpose $ mkTransformation (axisAngle (V3 0 0 1) elapsedTime) (V3 (sin elapsedTime) 0 0.1)
  let viewMatrix = transpose $ lookAt (V3 2 2 2) (V3 0 0 0) (V3 0 0 1)
  let scaleMatrix sx sy sz = V4 (V4 sx 0 0 0) (V4 0 sy 0 0) (V4 0 0 sz 0) (V4 0 0 0 1)
  let aspect = fromIntegral width / fromIntegral height
  let projMatrix = transpose $ zeroOnePerspective (45 * 3.14159 / 180.0) aspect 0.1 10 !*! scaleMatrix 1 (-1) 1
  let newUniforms = UBO modelMatrix viewMatrix projMatrix
  updateUniformBuffer ub newUniforms

updateUniformBuffer :: (InVulkanMonad effs) => UBuffer -> UniformBufferObject -> Eff effs ()
updateUniformBuffer (UBuffer _ alloc) newUniforms = do
  d <- getDevice
  let (MemoryAllocation memHandle _ _ (MemoryBlock _ bSize bOffset)) = alloc
  sendM $ VK.withMappedMemory d memHandle bOffset bSize VZ.zero bracket $ \ptr -> poke (castPtr ptr) newUniforms


makeDescriptorPool :: (InVulkanMonad effs) => Int -> Maybe AllocationCallbacks -> Eff effs DescriptorPool
makeDescriptorPool count allocator = do
  d <- getDevice
  let uniformPoolSize = VK.DescriptorPoolSize VK.DESCRIPTOR_TYPE_UNIFORM_BUFFER (fromIntegral count)
  let imagePoolSize = VK.DescriptorPoolSize VK.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER (fromIntegral count)
  let dCreateInfo = VK.DescriptorPoolCreateInfo () VZ.zero (fromIntegral count) (fromList [uniformPoolSize, imagePoolSize])
  VK.createDescriptorPool d dCreateInfo allocator

unmakeDescriptorPool :: (InVulkanMonad effs) => DescriptorPool -> Maybe AllocationCallbacks -> Eff effs ()
unmakeDescriptorPool dPool alloc = do
  d <- getDevice
  VK.destroyDescriptorPool d dPool alloc


makeDescriptorSets :: (InVulkanMonad effs) => DescriptorPool -> DescriptorSetLayout -> Int -> Eff effs (Vector DescriptorSet)
makeDescriptorSets dPool dLayout count = do
  d <- getDevice
  let layouts = replicate count dLayout
  let allocateInfo = VK.DescriptorSetAllocateInfo () dPool (fromList layouts)
  VK.allocateDescriptorSets d allocateInfo


-- | Descriptor set are automatically freed when their pool is destroyed, so this function is optional
unmakeDescriptorSets :: (InVulkanMonad effs) => DescriptorPool -> Vector DescriptorSet -> Eff effs ()
unmakeDescriptorSets dPool dSets = do
  d <- getDevice
  VK.freeDescriptorSets d dPool dSets



syncDescriptorSet :: (InVulkanMonad effs) => UBuffer -> ImageBuffer -> DescriptorSet -> Eff effs ()
syncDescriptorSet (UBuffer dBuf _) (ImageBuffer _ _ _ iView iSampler) dSet = do
  d <- getDevice
  let bufferInfo = VK.DescriptorBufferInfo dBuf 0 (fromIntegral $ sizeOf (undefined :: UniformBufferObject))
  let imageInfo = VK.DescriptorImageInfo iSampler iView VK.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
  let writeDescriptor1 = SomeStruct $ VK.WriteDescriptorSet () dSet 0 0 1 VK.DESCRIPTOR_TYPE_UNIFORM_BUFFER          empty (fromList [bufferInfo]) empty
  let writeDescriptor2 = SomeStruct $ VK.WriteDescriptorSet () dSet 1 0 1 VK.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER (fromList [imageInfo]) empty empty
  VK.updateDescriptorSets d (fromList [writeDescriptor1, writeDescriptor2]) empty


