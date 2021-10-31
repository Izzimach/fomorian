{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


-- | Module to handle image buffers: Image, ImageView, FrameBuffer, an alloc/dealloc of memory
module Fomorian.Vulkan.Resources.ImageBuffers where


import Data.Bits as Bits ((.&.), (.|.), zeroBits)
import Data.Row
import qualified Data.Map as M
import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VST
import Data.Word (Word8)

import Linear

import Control.Exception
import Control.Monad (forM_)
import Control.Monad.Freer as CMF

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array

import Vulkan.Core10 (Buffer, BufferUsageFlags, BufferCreateInfo(..), CommandPool, DeviceSize, PhysicalDevice, Format, Image, ImageView, ImageLayout, ImageTiling, Queue)
import qualified Vulkan.Core10 as VK
import qualified Vulkan.Zero as VZ
import Vulkan.CStruct.Extends

import qualified Codec.Picture as JUICY
import qualified Codec.Picture.Types as JC


import Fomorian.SceneResources
import Fomorian.SimpleMemoryArena

import Fomorian.Vulkan.VulkanMonads
import Fomorian.Vulkan.Resources.DataBuffers (loadStagingBuffer, destroyStagingBuffer)
import Fomorian.Vulkan.Resources.DeviceMemoryTypes (AbstractMemoryType(..))
import Fomorian.Vulkan.Resources.DeviceMemoryAllocator
import Fomorian.Vulkan.Resources.VulkanResourcesBase

--
-- images
--

loadImageFromFile :: (LastMember IO effs) => FilePath -> Eff effs (Either String (JUICY.Image JUICY.PixelRGBA8, Int, Int))
loadImageFromFile filePath = do
  dImage <- sendM $ JUICY.readImage filePath
  case dImage of
    Left errorMessage -> return $ Left $ "Error loading file " ++ filePath ++ ": " ++ errorMessage  
    Right img -> do
      let w = JUICY.dynamicMap JUICY.imageWidth img
      let h = JUICY.dynamicMap JUICY.imageHeight img
      let stdImage = toRGBA8 img
      return $ Right (stdImage, w, h)
  where
    toRGBA8 :: JUICY.DynamicImage -> JUICY.Image JUICY.PixelRGBA8
    toRGBA8 (JC.ImageRGBA8 img) = JC.promoteImage img
    toRGBA8 (JC.ImageRGB8 img)  = JC.promoteImage img
    toRGBA8 _                   = undefined


makeImagePrimitives :: (InVulkanMonad effs) => (Int,Int,Int) -> VK.SampleCountFlagBits -> Format -> ImageTiling -> VK.ImageUsageFlags -> VK.ImageAspectFlags -> VK.MemoryPropertyFlags -> Maybe VK.AllocationCallbacks -> Eff effs (Image,MemoryAllocation DeviceSize, ImageView)
makeImagePrimitives (w,h,mipmaps) numSamples imgFormat imgTiling imgUsage imgAspect _memProps allocator = do
  d <- getDevice
  let imgInfo = VK.ImageCreateInfo ()
                  VZ.zero
                  VK.IMAGE_TYPE_2D
                  imgFormat
                  (VK.Extent3D (fromIntegral w) (fromIntegral h) 1)
                  (fromIntegral mipmaps) -- mipLevels
                  1 -- arrayLayers
                  numSamples -- sample
                  imgTiling --IMAGE_TILING_OPTIMAL
                  imgUsage --(IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT)
                  VK.SHARING_MODE_EXCLUSIVE
                  (V.fromList [])
                  VK.IMAGE_LAYOUT_UNDEFINED
  imageHandle <- VK.createImage d imgInfo allocator
  memReq <- VK.getImageMemoryRequirements d imageHandle
  sendM $ print memReq
  pd <- getPhysicalDevice
  memAlloc <- allocateV memReq PreferGPU
  let (MemoryAllocation memHandle _ _ block) = memAlloc
  VK.bindImageMemory d imageHandle memHandle (blockOffset block)
  let viewInfo = VK.ImageViewCreateInfo () VZ.zero imageHandle VK.IMAGE_VIEW_TYPE_2D imgFormat VZ.zero (VK.ImageSubresourceRange imgAspect 0 (fromIntegral mipmaps) 0 1)
  imgView <- VK.createImageView d viewInfo allocator
  return (imageHandle, memAlloc, imgView)


makeImageBuffer :: (InVulkanMonad effs) => (Int,Int,Int) -> Format -> ImageTiling -> VK.ImageUsageFlags -> VK.MemoryPropertyFlags -> Maybe VK.AllocationCallbacks -> Eff effs ImageBuffer
makeImageBuffer (w,h,mipLevels) imgFormat imgTiling imgUsage memoryProps allocator = do
  (imageHandle, iMem, imgView) <- makeImagePrimitives (w,h,mipLevels) VK.SAMPLE_COUNT_1_BIT imgFormat imgTiling imgUsage VK.IMAGE_ASPECT_COLOR_BIT memoryProps allocator
  d <- getDevice
  phy <- getPhysicalDevice
  phyProps <- VK.getPhysicalDeviceProperties phy
  let samplerInfo = VK.SamplerCreateInfo () VZ.zero 
                      VK.FILTER_LINEAR -- magFilter
                      VK.FILTER_LINEAR -- minFilter
                      VK.SAMPLER_MIPMAP_MODE_LINEAR -- mapmapMode
                      VK.SAMPLER_ADDRESS_MODE_REPEAT -- addressModeU
                      VK.SAMPLER_ADDRESS_MODE_REPEAT -- addressModeV
                      VK.SAMPLER_ADDRESS_MODE_REPEAT -- addressModeW
                      0.0 -- mipLodBias
                      True -- anisotropyEnable-
                      (VK.maxSamplerAnisotropy $ VK.limits phyProps)  -- maxAnisotropy
                      False -- compareEnable
                      VK.COMPARE_OP_ALWAYS -- compareOp
                      0.0 -- minLod
                      (fromIntegral mipLevels) -- maxLod
                      VK.BORDER_COLOR_INT_OPAQUE_BLACK -- borderColor
                      False -- unnormalizedCoordinates
  imgSampler <- VK.createSampler d samplerInfo allocator
  return (ImageBuffer imageHandle mipLevels iMem imgView imgSampler)

unmakeImageBuffer :: (InVulkanMonad effs) => ImageBuffer -> Maybe VK.AllocationCallbacks -> Eff effs ()
unmakeImageBuffer (ImageBuffer imgHandle _mipmaps imgMem imgView imgSampler) allocator = do
  d <- getDevice
  VK.destroySampler d imgSampler allocator
  VK.destroyImageView d imgView allocator
  VK.destroyImage d imgHandle allocator
  deallocateV imgMem


makeTextureImage :: (InVulkanMonad effs) => FilePath -> Maybe VK.AllocationCallbacks -> Eff effs ImageBuffer
makeTextureImage filePath allocator = do
  d <- getDevice
  phy <- getPhysicalDevice
  imgBuffer@(ImageBuffer img _ _ _ _) <- makeImageBuffer (16,16,1)
                                                VK.FORMAT_R8G8B8A8_SRGB
                                                VK.IMAGE_TILING_OPTIMAL 
                                                (VK.IMAGE_USAGE_TRANSFER_SRC_BIT .|. VK.IMAGE_USAGE_TRANSFER_DST_BIT .|. VK.IMAGE_USAGE_SAMPLED_BIT) 
                                                VK.MEMORY_PROPERTY_DEVICE_LOCAL_BIT 
                                                allocator
  return imgBuffer
{-  imageResult <- loadImageFromFile filePath
  case imageResult of
    Left errString -> error errString
    Right (imgRGBA, w, h) -> do
      let imageByteSize = fromIntegral (w * h * 4)
      let mipLevels = 1 -- + floor (logBase 2.0 (fromIntegral $ max w h))
      let imagePixels = JC.imageData imgRGBA :: VST.Vector Word8
      let imageBytes = VST.toList imagePixels
      --staging@(StagingBuffer sBuf _) <- loadStagingBuffer imageBytes VK.BUFFER_USAGE_TRANSFER_SRC_BIT
      imgBuffer@(ImageBuffer img _ _ _ _) <- makeImageBuffer (w, h, mipLevels)
                                                VK.FORMAT_R8G8B8A8_SRGB
                                                VK.IMAGE_TILING_OPTIMAL 
                                                (VK.IMAGE_USAGE_TRANSFER_SRC_BIT .|. VK.IMAGE_USAGE_TRANSFER_DST_BIT .|. VK.IMAGE_USAGE_SAMPLED_BIT) 
                                                VK.MEMORY_PROPERTY_DEVICE_LOCAL_BIT 
                                                allocator
      --transitionImageLayout img mipLevels VK.FORMAT_R8G8B8A8_SRGB VK.IMAGE_LAYOUT_UNDEFINED VK.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      --copyBufferToImage sBuf img w h
      --generateMipmaps img VK.FORMAT_R8G8B8A8_SRGB w h  mipLevels
      --transitionImageLayout img mipLevels VK.FORMAT_R8G8B8A8_SRGB VK.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL VK.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
      --destroyStagingBuffer staging
      return imgBuffer-}


unmakeTextureImage :: (InVulkanMonad effs) => ImageBuffer -> Eff effs ()
unmakeTextureImage image = unmakeImageBuffer image Nothing

transitionImageLayout :: (InVulkanMonad effs, Member OneShotSubmitter effs) => Image -> Int -> Format -> ImageLayout -> ImageLayout -> Eff effs ()
transitionImageLayout img mipLevels format oldLayout newLayout = do
  d <- getDevice
  oneShotCommand $ \cBuf -> do
    let (srcMask, dstMask,srcStage,dstStage) = computeBarrierValues oldLayout newLayout
    let barrier = VK.ImageMemoryBarrier () srcMask dstMask oldLayout newLayout VK.QUEUE_FAMILY_IGNORED VK.QUEUE_FAMILY_IGNORED img (VK.ImageSubresourceRange VK.IMAGE_ASPECT_COLOR_BIT 0 (fromIntegral mipLevels) 0 1)
    VK.cmdPipelineBarrier cBuf srcStage dstStage VZ.zero V.empty V.empty (V.fromList [SomeStruct barrier])
    return ()
  where
    computeBarrierValues VK.IMAGE_LAYOUT_UNDEFINED VK.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL          = (VZ.zero, VK.ACCESS_TRANSFER_WRITE_BIT,
                                                                                                    VK.PIPELINE_STAGE_TOP_OF_PIPE_BIT,
                                                                                                    VK.PIPELINE_STAGE_TRANSFER_BIT)
    computeBarrierValues VK.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL VK.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = (VK.ACCESS_TRANSFER_WRITE_BIT, VK.ACCESS_SHADER_READ_BIT,
                                                                                                          VK.PIPELINE_STAGE_TRANSFER_BIT,
                                                                                                          VK.PIPELINE_STAGE_FRAGMENT_SHADER_BIT)

copyBufferToImage :: (InVulkanMonad effs, Member OneShotSubmitter effs) => Buffer -> Image -> Int -> Int -> Eff effs ()
copyBufferToImage iBuf img width height = do
  oneShotCommand $ \cBuf -> do
    let copyInfo = VK.BufferImageCopy 0 0 0 (VK.ImageSubresourceLayers VK.IMAGE_ASPECT_COLOR_BIT 0 0 1) (VK.Offset3D 0 0 0) (VK.Extent3D (fromIntegral width) (fromIntegral height) 1)
    VK.cmdCopyBufferToImage cBuf iBuf img VK.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL (V.singleton copyInfo)


generateMipmaps :: (InVulkanMonad effs, Member OneShotSubmitter effs) => Image -> Format -> Int -> Int -> Int -> Eff effs ()
generateMipmaps img mipFormat w h mipLevels = do
  phy <- getPhysicalDevice
  formatProps <- VK.getPhysicalDeviceFormatProperties phy mipFormat
  if (VK.optimalTilingFeatures formatProps .&. VK.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT) > zeroBits
  then return ()
  else error "Linear filtering not supported!"
  oneShotCommand $ \cBuf -> do
    forM_ [1..(mipLevels-1)] $ \thisLevel -> do
      let baseLevel = thisLevel - 1
      let mipWidth = max 1 (w `div` (2 ^ baseLevel))
      let mipHeight = max 1 (h `div` (2 ^ baseLevel))
      let nextMipWidth = max 1 (mipWidth `div`2)
      let nextMipHeight = max 1 (mipHeight `div` 2)
      let barrier1 = SomeStruct $ VK.ImageMemoryBarrier () 
                        VK.ACCESS_TRANSFER_WRITE_BIT 
                        VK.ACCESS_TRANSFER_READ_BIT 
                        VK.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                        VK.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                        VK.QUEUE_FAMILY_IGNORED
                        VK.QUEUE_FAMILY_IGNORED
                        img
                        (VK.ImageSubresourceRange VK.IMAGE_ASPECT_COLOR_BIT (fromIntegral baseLevel) 1 0 1)
      VK.cmdPipelineBarrier cBuf VK.PIPELINE_STAGE_TRANSFER_BIT VK.PIPELINE_STAGE_TRANSFER_BIT VZ.zero V.empty V.empty (V.fromList [barrier1])                       
      let blit = VK.ImageBlit
                   (VK.ImageSubresourceLayers VK.IMAGE_ASPECT_COLOR_BIT (fromIntegral baseLevel) 0 1)
                   (VK.Offset3D 0 0 0, VK.Offset3D (fromIntegral mipWidth) (fromIntegral mipHeight) 1)
                   (VK.ImageSubresourceLayers VK.IMAGE_ASPECT_COLOR_BIT (fromIntegral thisLevel) 0 1)
                   (VK.Offset3D 0 0 0, VK.Offset3D (fromIntegral nextMipWidth) (fromIntegral nextMipHeight) 1)
      VK.cmdBlitImage cBuf img VK.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL img VK.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL (V.fromList [blit]) VK.FILTER_LINEAR
      let barrier2 = SomeStruct $ VK.ImageMemoryBarrier () 
                        VK.ACCESS_TRANSFER_READ_BIT 
                        VK.ACCESS_SHADER_READ_BIT 
                        VK.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                        VK.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                        VK.QUEUE_FAMILY_IGNORED
                        VK.QUEUE_FAMILY_IGNORED
                        img
                        (VK.ImageSubresourceRange VK.IMAGE_ASPECT_COLOR_BIT (fromIntegral baseLevel) 1 0 1)
      VK.cmdPipelineBarrier cBuf VK.PIPELINE_STAGE_TRANSFER_BIT VK.PIPELINE_STAGE_FRAGMENT_SHADER_BIT VZ.zero V.empty V.empty (V.fromList [barrier2])
    -- outside the forM_
    let barrier3 = SomeStruct $ VK.ImageMemoryBarrier () 
                        VK.ACCESS_TRANSFER_WRITE_BIT 
                        VK.ACCESS_SHADER_READ_BIT 
                        VK.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                        VK.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                        VK.QUEUE_FAMILY_IGNORED
                        VK.QUEUE_FAMILY_IGNORED
                        img
                        (VK.ImageSubresourceRange VK.IMAGE_ASPECT_COLOR_BIT (fromIntegral (mipLevels-1)) 1 0 1)
    VK.cmdPipelineBarrier cBuf VK.PIPELINE_STAGE_TRANSFER_BIT VK.PIPELINE_STAGE_FRAGMENT_SHADER_BIT VZ.zero V.empty V.empty (V.fromList [barrier3])  


{-
makeColorBuffer :: Device -> PhysicalDevice -> Int -> Int -> SampleCountFlagBits -> Format -> Maybe AllocationCallbacks -> IO ColorBuffer
makeColorBuffer device phy w h numSamples colorFormat allocator = do
  (img, iMem, imgView) <- makeImageParts device phy w h 1 numSamples colorFormat
                              IMAGE_TILING_OPTIMAL
                              (IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT .|. IMAGE_USAGE_COLOR_ATTACHMENT_BIT)
                              IMAGE_ASPECT_COLOR_BIT
                              MEMORY_PROPERTY_DEVICE_LOCAL_BIT
                              allocator
  return (ColorBuffer img iMem imgView)


unmakeColorBuffer :: Device -> ColorBuffer -> Maybe AllocationCallbacks -> IO ()
unmakeColorBuffer device (ColorBuffer cBuf cMem cView) allocator = do
  destroyImageView device cView allocator
  destroyImage device cBuf allocator
  freeMemory device cMem allocator
-}

makeDepthBuffer ::  (InVulkanMonad effs) => (Int, Int) -> VK.SampleCountFlagBits -> Maybe VK.AllocationCallbacks -> Eff effs DepthBuffer
makeDepthBuffer (w,h) numSamples allocator = do
  depthFormat <- findDepthFormat
  (img, iMem, imgView) <- makeImagePrimitives (w,h,1) numSamples depthFormat
                              VK.IMAGE_TILING_OPTIMAL
                              VK.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
                              VK.IMAGE_ASPECT_DEPTH_BIT
                              VK.MEMORY_PROPERTY_DEVICE_LOCAL_BIT
                              allocator
  return (DepthBuffer img iMem imgView)

destroyDepthBuffer :: (InVulkanMonad effs) => DepthBuffer -> Maybe VK.AllocationCallbacks -> Eff effs ()
destroyDepthBuffer (DepthBuffer img imgMem imgView) allocator = do
  d <- getDevice
  VK.destroyImageView d imgView allocator
  VK.destroyImage d img allocator
  deallocateV imgMem

findDepthFormat :: (InVulkanMonad effs) => Eff effs Format
findDepthFormat =
  findSupportedFormat
    (V.fromList [VK.FORMAT_D32_SFLOAT, VK.FORMAT_D32_SFLOAT_S8_UINT, VK.FORMAT_D24_UNORM_S8_UINT])
    VK.IMAGE_TILING_OPTIMAL
    VK.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT
  
hasStencilComponent :: Format -> Bool
hasStencilComponent f = (f == VK.FORMAT_D32_SFLOAT_S8_UINT) || (f == VK.FORMAT_D24_UNORM_S8_UINT)

findSupportedFormat :: (InVulkanMonad effs) => V.Vector Format -> ImageTiling -> VK.FormatFeatureFlags -> Eff effs Format
findSupportedFormat possibleFormats tiling chooseFeatures = do
  phy <- getPhysicalDevice
  vecFeatures <- mapM (relevantFeatures phy) possibleFormats
  case V.findIndex hasFeatures vecFeatures of
    Nothing -> error "No allowed format"
    Just foundIndex -> return (possibleFormats ! foundIndex)
  where
    relevantFeatures ph fmt = do
      props <- VK.getPhysicalDeviceFormatProperties ph fmt
      case tiling of
        VK.IMAGE_TILING_OPTIMAL -> return $ VK.optimalTilingFeatures props
        VK.IMAGE_TILING_LINEAR -> return $ VK.linearTilingFeatures props
        _ -> return VZ.zero
    hasFeatures features = (features .&. chooseFeatures) > VK.FormatFeatureFlagBits VZ.zero
