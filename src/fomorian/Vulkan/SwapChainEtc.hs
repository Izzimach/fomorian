{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Fomorian.Vulkan.SwapChainEtc (
  SwapChainEtc(..),
  DeviceEtc(..),
  TransientResources(..),
  withSwapChainEtc,
  generateSwapChainInfo,
  recreateSwapChainEtc
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString (readFile)
import Data.Foldable
import Data.IORef
import Data.Vector ((!), Vector, empty, fromList)
import Data.Word (Word16, Word32)
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Linear (V2 (..), V3 (..))
import System.FilePath
import Vulkan.CStruct.Extends
import Vulkan.Core10 as VKCORE
import Vulkan.Core10.MemoryManagement as VKMEM
import Vulkan.Extensions.VK_KHR_surface as VKSURFACE
import Vulkan.Extensions.VK_KHR_swapchain as VKSWAPCHAIN
import Vulkan.Zero

import Fomorian.Vulkan.TransientResources

-- | Holds a swapchain and the associated data handles.
data SwapChainEtc = SwapChainEtc
  { theSwapchain :: SwapchainKHR,
    swapchainImages :: Vector Image,
    swapchainImageViews :: Vector ImageView,
    swapchainFramebuffers :: Vector Framebuffer,
    swapchainCommandBuffers :: Vector CommandBuffer,
    swapchainPerFrameResources :: Vector ImageFrameResources,
    swapchainDescriptorPool :: DescriptorPool,
    swapchainDescriptorSets :: Vector DescriptorSet,
    swapchainDescriptorSetLayoutInstance :: DescriptorSetLayout,
    swapchainPipeline :: PipelineEtc,
    swapchainCreateInfo :: SwapchainCreateInfoKHR '[]
  }
  deriving (Show)

data DeviceEtc = DeviceEtc
  { physicalHandle :: PhysicalDevice,
    graphicsQueueIndex :: Word32,
    presentQueueIndex :: Word32
  }
  deriving (Eq, Show)

-- | Get all the info for a given surface: capabilities, formats, and present modes.
--   Returns a swapchain create info struct with a NULL oldswapchain value.
generateSwapChainInfo :: DeviceEtc -> SurfaceKHR -> IO (SwapchainCreateInfoKHR '[])
generateSwapChainInfo (DeviceEtc d gq pq) s = do
  capabilities <- getPhysicalDeviceSurfaceCapabilitiesKHR d s
  (SUCCESS, formats) <- getPhysicalDeviceSurfaceFormatsKHR d s
  (SUCCESS, pmodes) <- getPhysicalDeviceSurfacePresentModesKHR d s
  let desiredSwapChainImages = chooseSwapChainImageCount capabilities
  let presentFormat = choosePresentationFormat formats
  let swapChainSize = chooseSwapChainImageSize capabilities
  let (usageBits, transformBits) = checkSwapChainUsageAndTransform capabilities
  let presentingMode = choosePresentMode pmodes
  let whatImageSharingMode = if (gq == pq) then SHARING_MODE_EXCLUSIVE else SHARING_MODE_CONCURRENT
  let whatQueueFamilyIndices = if (gq == pq) then empty else (fromList [gq, pq])
  return $
    SwapchainCreateInfoKHR
      () -- pNext
      zero -- swapchain create flags
      s -- surface
      desiredSwapChainImages -- minImageCount
      (VKSURFACE.format presentFormat) -- imageFormat
      (VKSURFACE.colorSpace presentFormat) -- imageColorSpace
      swapChainSize -- imageExtent
      1 -- imageArrayLayers
      usageBits -- imageUsage
      whatImageSharingMode -- imageSharingMode
      whatQueueFamilyIndices -- queueFamilyIndices if sharing is concurrent
      transformBits -- preTransform
      COMPOSITE_ALPHA_OPAQUE_BIT_KHR -- compositeAlpha
      presentingMode -- presentMode
      True -- clipped
      NULL_HANDLE

-- | Creates a swapchain and passes an IORef to the wrapped user function. We provide an IORef because
--   sometimes you have to recreate the swapchain in-place without restarting the program.
withSwapChainEtc :: Device -> PhysicalDevice -> CommandPool -> TransientResources -> Queue -> SwapchainCreateInfoKHR '[] -> Maybe AllocationCallbacks -> (IO (IORef SwapChainEtc) -> (IORef SwapChainEtc -> IO ()) -> r) -> r
withSwapChainEtc device phy cPool tRes gq createInfo allocator wrapper = wrapper startSwapchainRef endSwapchainRef
  where
    startSwapchainRef =
      do
        swETC <- createSwapChainEtc device phy cPool tRes createInfo allocator
        swRef <- liftIO $ newIORef swETC
        return swRef
    endSwapchainRef swRef =
      do
        swETC <- liftIO $ readIORef swRef
        destroySwapChainEtc device cPool allocator swETC

createSwapChainEtc :: Device -> PhysicalDevice -> CommandPool -> TransientResources -> SwapchainCreateInfoKHR '[] -> Maybe AllocationCallbacks -> IO SwapChainEtc
createSwapChainEtc device phy cpool tRes createInfo allocator = do
  newSwapchain <- createSwapchainKHR device createInfo allocator
  (_, newImages) <- getSwapchainImagesKHR device newSwapchain
  newImageViews <- createImageViews device createInfo newImages
  descriptorSetLayoutInstance <- makeDescriptorSetLayout device allocator
  dPool <- makeDescriptorPool device (length newImages) allocator
  dSets <- makeDescriptorSets device dPool descriptorSetLayoutInstance (length newImages)
  fRes <- makeImageFrameResources device phy newImages allocator
  let uBufs = fmap uniforms fRes
  let imgBuffer = textureImage tRes
  syncDescriptorSets device uBufs imgBuffer dSets
  newPipe <- liftIO $ buildSimplePipeline device allocator descriptorSetLayoutInstance createInfo
  framebuffers <- makeFramebuffers device newPipe createInfo newImageViews
  cmdBuffers <- makeCommandBuffers device cpool framebuffers
  recordCommandBuffers cmdBuffers framebuffers tRes createInfo newPipe dSets
  return (SwapChainEtc newSwapchain newImages newImageViews framebuffers cmdBuffers fRes dPool dSets descriptorSetLayoutInstance newPipe createInfo)

destroySwapChainEtc :: Device -> CommandPool -> Maybe AllocationCallbacks -> SwapChainEtc -> IO ()
destroySwapChainEtc device cpool allocator swETC = do
  -- destroy in reverse order from creation
  let (SwapChainEtc sc _ imageViews framebuffers commandbuffers frameresources dPool _dSets dLayout pipe _) = swETC
  destroyImageFrameResources device frameresources allocator
  freeCommandBuffers device cpool commandbuffers
  mapM (\fb -> destroyFramebuffer device fb Nothing) framebuffers
  destroyPipelineEtc device pipe allocator
  unmakeDescriptorPool device dPool allocator
  -- don't need to destory descriptor sets, they get destroyed when the pool is destroyed
  unmakeDescriptorSetLayout device dLayout allocator
  mapM (\iv -> destroyImageView device iv Nothing) imageViews
  destroySwapchainKHR device sc allocator

recreateSwapChainEtc :: Device -> PhysicalDevice -> CommandPool -> TransientResources -> SwapChainEtc -> DeviceEtc -> SurfaceKHR -> Maybe AllocationCallbacks -> IO SwapChainEtc
recreateSwapChainEtc device phy cpool tRes oldswETC chosen surfaceK allocator = do
  destroySwapChainEtc device cpool allocator oldswETC
  newCreateInfo <- liftIO $ generateSwapChainInfo chosen surfaceK
  createSwapChainEtc device phy cpool tRes newCreateInfo allocator


createImageViews :: Device -> SwapchainCreateInfoKHR '[] -> Vector Image -> IO (Vector ImageView)
createImageViews device swapchainInfo images =
  let idSw = COMPONENT_SWIZZLE_IDENTITY
      mkImageCreateInfo i =
        ImageViewCreateInfo
          ()
          zero
          i
          IMAGE_VIEW_TYPE_2D
          (imageFormat swapchainInfo)
          (ComponentMapping idSw idSw idSw idSw)
          (ImageSubresourceRange IMAGE_ASPECT_COLOR_BIT 0 1 0 1)
      mkView img = createImageView device (mkImageCreateInfo img) Nothing
   in traverse mkView images

makeFramebuffers :: Device -> PipelineEtc -> SwapchainCreateInfoKHR '[] -> Vector ImageView -> IO (Vector Framebuffer)
makeFramebuffers device pipe sce imageViews = do
  let (Extent2D w h) = (VKSWAPCHAIN.imageExtent $ sce)
  let rPass = rendPass pipe
  let mkFramebuffers iv = createFramebuffer device (FramebufferCreateInfo () zero rPass (fromList [iv]) w h 1) Nothing
  fbs <- mapM mkFramebuffers imageViews
  return fbs

makeCommandBuffers :: Device -> CommandPool -> Vector Framebuffer -> IO (Vector CommandBuffer)
makeCommandBuffers device cmdpool fb = do
  let count = fromIntegral $ length fb
  let allocInfo = CommandBufferAllocateInfo cmdpool COMMAND_BUFFER_LEVEL_PRIMARY count
  buffers <- allocateCommandBuffers device allocInfo
  return buffers

makeFrameResources :: Device -> CommandPool -> Vector ImageView -> IO (Vector ImageFrameResources)
makeFrameResources = undefined

destroyFrameResources :: Device -> Vector ImageFrameResources -> IO ()
destroyFrameResources = undefined

-- | Given some surface picks a suitable number of surfaces. This will:
--   - Try to use one more than the minimum number of images.
--   - If that exceeds the maximum allowed, uses the maximum allowed.
chooseSwapChainImageCount :: SurfaceCapabilitiesKHR -> Word32
chooseSwapChainImageCount s =
  let minImages = (VKSURFACE.minImageCount s)
      maxImages = (VKSURFACE.maxImageCount s)
      desiredImages = minImages + 1
   in -- if 'maxImages' is 0 there is no upper limit so we don't need to clamp
      if (maxImages == 0)
        then desiredImages
        else min desiredImages maxImages

-- | Picks a format. Tries to find an R8G8B8_UNORM format, but if that's
--   not found just picks the first format in the list.
choosePresentationFormat :: Vector SurfaceFormatKHR -> SurfaceFormatKHR
choosePresentationFormat fs =
  let desiredFormat = FORMAT_R8G8B8A8_SRGB
      desiredColorspace = COLOR_SPACE_SRGB_NONLINEAR_KHR
      formatCount = length fs
      hasFormat =
        ( \f ->
            (VKSURFACE.format f == desiredFormat)
              && (VKSURFACE.colorSpace f == desiredColorspace)
        )
   in -- the driver can throw up it's hands (if it had hands) and say "idc what format you use"
      -- so we check for that first
      if (formatCount == 1 && ((VKSURFACE.format (fs ! 0)) == FORMAT_UNDEFINED))
        then (SurfaceFormatKHR desiredFormat desiredColorspace)
        else case (find hasFormat fs) of
          -- if we found a good format use it, otherwise settle for the first format
          Just f' -> f'
          Nothing -> fs ! 0

chooseSwapChainImageSize :: SurfaceCapabilitiesKHR -> Extent2D
chooseSwapChainImageSize s =
  let (Extent2D w _h) = currentExtent s
   in -- use whatever currentExtent is. If currentExtent is -1 we have to choose our own extent
      if (w /= maxBound)
        then currentExtent s
        else
          let (Extent2D minW minH) = minImageExtent s
              (Extent2D maxW maxH) = maxImageExtent s
              chooseW = max minW (min maxW 640)
              chooseH = max minH (min maxH 480)
           in (Extent2D chooseW chooseH)

checkSwapChainUsageAndTransform :: SurfaceCapabilitiesKHR -> (ImageUsageFlagBits, SurfaceTransformFlagBitsKHR)
checkSwapChainUsageAndTransform s =
  let hasBits = supportedUsageFlags s
      needBits =
        [ IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
          IMAGE_USAGE_TRANSFER_DST_BIT
        ]
      checkBit = \bitX -> if (hasBits .&. bitX) == zero then ["usage not supported: " ++ show bitX] else []
      checks = concat $ map checkBit needBits
      allBits = foldl1 (.|.) needBits
   in --transformBits = supportedTransforms s
      if (length checks > 0)
        then error $ "Cannot create swapchain. " ++ (concat checks)
        else (allBits, SURFACE_TRANSFORM_IDENTITY_BIT_KHR)

choosePresentMode :: Vector PresentModeKHR -> PresentModeKHR
choosePresentMode pmodes =
  -- use fifo mode if found, otherwise error
  case (find (== PRESENT_MODE_FIFO_KHR) pmodes) of
    Nothing -> error "No FIFO presentation mode found"
    Just m -> m
