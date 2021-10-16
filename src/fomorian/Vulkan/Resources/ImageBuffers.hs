{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


-- | Module to handle image buffers: Image, ImageView, FrameBuffer, an alloc/dealloc of memory
module Fomorian.Vulkan.Resources.ImageBuffers where

import Data.Bits ((.&.))
import Data.Row
import qualified Data.Map as M
import Data.Vector ((!))
import qualified Data.Vector as V

import Linear

import Control.Exception
import Control.Monad.Freer as CMF

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array

import Vulkan.Core10 (Buffer, BufferUsageFlags, BufferCreateInfo(..), DeviceSize, PhysicalDevice, Format, Image, ImageView, ImageTiling)
import qualified Vulkan.Core10 as VK
import qualified Vulkan.Zero as VZ

import Fomorian.SceneResources
import Fomorian.SimpleMemoryArena

import Fomorian.Vulkan.VulkanMonads
import Fomorian.Vulkan.Resources.DeviceMemoryTypes (AbstractMemoryType(..))
import Fomorian.Vulkan.Resources.DeviceMemoryAllocator
import Fomorian.Vulkan.Resources.VulkanResourcesBase

--
-- images
--
{-
loadImageFromFile :: FilePath -> IO (Either String (JUICY.Image JUICY.PixelRGBA8, Int, Int))
loadImageFromFile filePath = do
  dImage <- JUICY.readImage filePath
  case dImage of
    Left errorMessage -> return $ Left $ "Error loading file " ++ filePath ++ ": " ++ errorMessage  
    Right img -> do
      let w = JUICY.dynamicMap JUICY.imageWidth img
      let h = JUICY.dynamicMap JUICY.imageHeight img
      let stdImage = toRGBA8 img
      return $ Right $ (stdImage, w, h)
  where
    toRGBA8 :: JUICY.DynamicImage -> JUICY.Image JUICY.PixelRGBA8
    toRGBA8 (JC.ImageRGBA8 img) = JC.promoteImage img
    toRGBA8 (JC.ImageRGB8 img)  = JC.promoteImage img
    toRGBA8 _                   = undefined
-}

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
  memAlloc <- allocateV memReq PreferGPU
  let (MemoryAllocation memHandle _ _ block) = memAlloc
  VK.bindImageMemory d imageHandle memHandle (blockOffset block)
  let viewInfo = VK.ImageViewCreateInfo () VZ.zero imageHandle VK.IMAGE_VIEW_TYPE_2D imgFormat VZ.zero (VK.ImageSubresourceRange imgAspect 0 (fromIntegral mipmaps) 0 1)
  imgView <- VK.createImageView d viewInfo allocator
  return (imageHandle, memAlloc, imgView)

{-
makeIMGBuffer :: Device -> PhysicalDevice -> Int -> Int -> Int -> Format -> ImageTiling -> ImageUsageFlags -> MemoryPropertyFlags -> Maybe AllocationCallbacks -> IO IMGBuffer
makeIMGBuffer device phy w h mipLevels imgFormat imgTiling imgUsage memoryProps allocator = do
  (imageHandle, iMem, imgView) <- makeImageParts device phy w h mipLevels SAMPLE_COUNT_1_BIT imgFormat imgTiling imgUsage IMAGE_ASPECT_COLOR_BIT memoryProps allocator
  phyProps <- getPhysicalDeviceProperties phy
  let samplerInfo = SamplerCreateInfo () zero 
                      FILTER_LINEAR -- magFilter
                      FILTER_LINEAR -- minFilter
                      SAMPLER_MIPMAP_MODE_LINEAR -- mapmapMode
                      SAMPLER_ADDRESS_MODE_REPEAT -- addressModeU
                      SAMPLER_ADDRESS_MODE_REPEAT -- addressModeV
                      SAMPLER_ADDRESS_MODE_REPEAT -- addressModeW
                      0.0 -- mipLodBias
                      True -- anisotropyEnable
                      (maxSamplerAnisotropy $ limits phyProps)  -- maxAnisotropy
                      False -- compareEnable
                      COMPARE_OP_ALWAYS -- compareOp
                      0.0 -- minLod
                      (fromIntegral mipLevels) -- maxLod
                      BORDER_COLOR_INT_OPAQUE_BLACK -- borderColor
                      False -- unnormalizedCoordinates
  imgSampler <- createSampler device samplerInfo allocator
  return (IMGBuffer imageHandle mipLevels iMem imgView imgSampler)

unmakeIMGBuffer :: Device -> IMGBuffer -> Maybe AllocationCallbacks -> IO ()
unmakeIMGBuffer device (IMGBuffer imgHandle _mipmaps imgMem imgView imgSampler) allocator = do
  destroySampler device imgSampler allocator
  destroyImageView device imgView allocator
  destroyImage device imgHandle allocator
  freeMemory device imgMem allocator
  return ()


makeTextureImage :: Device -> PhysicalDevice -> CommandPool -> Queue -> FilePath -> Maybe AllocationCallbacks -> IO IMGBuffer
makeTextureImage device phy cPool gQueue filePath allocator = do
  imageResult <- partialLoadImage filePath
  case imageResult of
    Left errString -> fail errString
    Right (imgRGBA, w, h) -> do
      let imageByteSize = fromIntegral (w * h * 4)
      let mipLevels = 1 + (floor (log (fromIntegral $ max w h) / log 2.0))
      let imagePixels = (JC.imageData imgRGBA) :: VS.Vector Word8
      let imageBytes = VS.toList imagePixels
      staging@(StagingBuffer sBuf _) <- createStagingBuffer device phy imageBytes allocator
      imgBuffer@(IMGBuffer img _ _ _ _) <- makeIMGBuffer device phy w h mipLevels 
                                              FORMAT_R8G8B8A8_SRGB
                                              IMAGE_TILING_OPTIMAL 
                                              (IMAGE_USAGE_TRANSFER_SRC_BIT .|. IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT) 
                                              MEMORY_PROPERTY_DEVICE_LOCAL_BIT 
                                              allocator
      
      transitionImageLayout device cPool gQueue img mipLevels FORMAT_R8G8B8A8_SRGB IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      copyBufferToImage device cPool gQueue sBuf img w h
      generateMipmaps device phy cPool gQueue img FORMAT_R8G8B8A8_SRGB w h  mipLevels
      --transitionImageLayout device cPool gQueue img mipLevels FORMAT_R8G8B8A8_SRGB IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
      destroyStagingBuffer device staging allocator
      return imgBuffer


unmakeTextureImage :: Device -> IMGBuffer -> Maybe AllocationCallbacks -> IO ()
unmakeTextureImage = unmakeIMGBuffer

transitionImageLayout :: Device -> CommandPool -> Queue -> Image -> Int -> Format -> ImageLayout -> ImageLayout -> IO ()
transitionImageLayout device cPool gQueue img mipLevels format oldLayout newLayout =
  withOneTimeCommand device cPool gQueue bracket $ \cBuf -> do
    let (srcMask, dstMask,srcStage,dstStage) = computeBarrierValues oldLayout newLayout
    let barrier = ImageMemoryBarrier () srcMask dstMask oldLayout newLayout QUEUE_FAMILY_IGNORED QUEUE_FAMILY_IGNORED img (ImageSubresourceRange IMAGE_ASPECT_COLOR_BIT 0 (fromIntegral mipLevels) 0 1)
    cmdPipelineBarrier cBuf srcStage dstStage zero empty empty (fromList [SomeStruct barrier])
    return ()
  where
    computeBarrierValues IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL                = (zero, ACCESS_TRANSFER_WRITE_BIT,
                                                                                                    PIPELINE_STAGE_TOP_OF_PIPE_BIT,
                                                                                                    PIPELINE_STAGE_TRANSFER_BIT)
    computeBarrierValues IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = (ACCESS_TRANSFER_WRITE_BIT,ACCESS_SHADER_READ_BIT,
                                                                                                    PIPELINE_STAGE_TRANSFER_BIT,
                                                                                                    PIPELINE_STAGE_FRAGMENT_SHADER_BIT)


copyBufferToImage :: Device -> CommandPool -> Queue -> Buffer -> Image -> Int -> Int -> IO ()
copyBufferToImage device cPool gQueue iBuf img width height =
  withOneTimeCommand device cPool gQueue bracket $ \cBuf -> do
    let copyInfo = BufferImageCopy 0 0 0 (ImageSubresourceLayers IMAGE_ASPECT_COLOR_BIT 0 0 1) (Offset3D 0 0 0) (Extent3D (fromIntegral width) (fromIntegral height) 1)
    cmdCopyBufferToImage cBuf iBuf img IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL (fromList [copyInfo])


data ColorBuffer = ColorBuffer Image DeviceMemory ImageView
  deriving Show

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
