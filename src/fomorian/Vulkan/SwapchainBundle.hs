{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Fomorian.Vulkan.SwapchainBundle where

import Control.Monad.Freer

import Data.Bits as Bits ((.|.), (.&.), zeroBits)
import Data.Vector as V ((!), Vector, fromList, empty, find)
import Data.Word (Word32)
import Data.ByteString (readFile)

import System.FilePath (FilePath(), (</>))

import Vulkan.Core10 (Device, Extent2D(..), Format, Image, ImageView, Framebuffer, CommandPool, CommandBuffer, RenderPass)
import qualified Vulkan.Core10 as VK
import qualified Vulkan.Zero as VZ
import Vulkan.Extensions (SwapchainKHR, SwapchainCreateInfoKHR, createSwapchainKHR, destroySwapchainKHR, getSwapchainImagesKHR, supportedUsageFlags, supportedUsageFlags)
import qualified Vulkan.Extensions.VK_KHR_surface as VKSURFACE
import qualified Vulkan.Extensions.VK_KHR_swapchain as VKSWAPCHAIN

import Fomorian.Vulkan.WindowBundle
import Fomorian.Vulkan.VulkanMonads
import Fomorian.Vulkan.Resources.VulkanResourcesBase (DepthBuffer(..))
import Fomorian.Vulkan.Resources.ImageBuffers (makeDepthBuffer, destroyDepthBuffer, findDepthFormat)
import Fomorian.Vulkan.Resources.Pipeline



-- | Each image of the swapchain has a bunch of other related data: the ImageView, a Framebuffer, and maybe others?
data SwapchainPerImageData = SwapchainPerImageData Image ImageView DepthBuffer Framebuffer
  deriving (Eq,Show)

data SwapchainPresentInfo = SwapchainPresentInfo Format Format Extent2D

data SwapchainBundle =
  SwapchainBundle
  {
    relevantCreateInfo :: SwapchainCreateInfoKHR '[],
    swapchainHandle :: SwapchainKHR,
    swapchainPresentInfo :: SwapchainPresentInfo,
    swapchainRenderPass :: VK.RenderPass,
    swapchainImages :: Vector SwapchainPerImageData
  }

-- | Makes a swapchain bundle. Pass in an (optional) previous swapchain to re-use some components from there.
makeSwapchainBundle :: (InVulkanMonad effs) => Maybe SwapchainBundle -> Eff effs SwapchainBundle
makeSwapchainBundle previousSwapchain = do
  let allocator = Nothing
  d <- getDevice
  createInfo <- makeSwapChainCreateInfo previousSwapchain
  newSwapchain <- createSwapchainKHR d createInfo allocator
  (_, newImages) <- getSwapchainImagesKHR d newSwapchain
  let ext = VKSWAPCHAIN.imageExtent createInfo
      fmt = VKSWAPCHAIN.imageFormat createInfo
  depthFormat <- findDepthFormat
  rPass <- makeSimpleRenderPass fmt depthFormat
  perImage <- mapM (makePerImageData ext rPass fmt) newImages
  let presentInfo = SwapchainPresentInfo fmt depthFormat ext
  return $ SwapchainBundle createInfo newSwapchain presentInfo rPass perImage
  
destroySwapchainBundle :: (InVulkanMonad effs) => SwapchainBundle -> Eff effs ()
destroySwapchainBundle scBundle = do
  mapM_ destroyPerImageData (swapchainImages scBundle)
  d <- getDevice
  VK.destroyRenderPass d (swapchainRenderPass scBundle) Nothing
  destroySwapchainKHR d (swapchainHandle scBundle) Nothing

makePerImageData :: (InVulkanMonad effs) => Extent2D -> RenderPass -> Format -> Image -> Eff effs SwapchainPerImageData
makePerImageData (Extent2D w h) rPass fmt img =
  let idSw = VK.COMPONENT_SWIZZLE_IDENTITY
      viewInfo = VK.ImageViewCreateInfo () VZ.zero img VK.IMAGE_VIEW_TYPE_2D fmt (VK.ComponentMapping idSw idSw idSw idSw) (VK.ImageSubresourceRange VK.IMAGE_ASPECT_COLOR_BIT 0 1 0 1)
  in do
    d <- getDevice
    imageView <- VK.createImageView d viewInfo Nothing
    depthBuf <- makeDepthBuffer (fromIntegral w, fromIntegral h) VK.SAMPLE_COUNT_1_BIT Nothing
    let (DepthBuffer _ _ depthImageView) = depthBuf
    frameBuf <- VK.createFramebuffer d (VK.FramebufferCreateInfo () VZ.zero rPass (fromList [imageView, depthImageView]) w h 1) Nothing
    return (SwapchainPerImageData img imageView depthBuf frameBuf)

destroyPerImageData :: (InVulkanMonad effs) => SwapchainPerImageData -> Eff effs ()
destroyPerImageData (SwapchainPerImageData _ imgView depthBuf frameBuf) = do
  d <- getDevice
  sendM $ VK.destroyFramebuffer d frameBuf Nothing
  destroyDepthBuffer depthBuf Nothing
  sendM $ VK.destroyImageView d imgView Nothing
  -- img is managed by the swapchain, so it isn't destroyed here




-- | Get all the info for a given surface: capabilities, formats, and present modes.
--   Returns a swapchain create info struct with a NULL oldswapchain value.
makeSwapChainCreateInfo :: (InVulkanMonad effs) => Maybe SwapchainBundle -> Eff effs (SwapchainCreateInfoKHR '[])
makeSwapChainCreateInfo previousSwapchain = do
  pd <- getPhysicalDevice
  s <- vulkanSurface <$> getWindowBundle
  capabilities <- VKSURFACE.getPhysicalDeviceSurfaceCapabilitiesKHR pd s
  (_fStatus, formats) <- VKSURFACE.getPhysicalDeviceSurfaceFormatsKHR pd s
  (_modeStatus, pmodes) <- VKSURFACE.getPhysicalDeviceSurfacePresentModesKHR pd s
  let desiredSwapChainImages = chooseSwapChainImageCount capabilities
  let presentFormat = choosePresentationFormat formats
  let swapChainSize = chooseSwapChainImageSize capabilities
  let (usageBits, transformBits) = checkSwapChainUsageAndTransform capabilities
  let presentingMode = choosePresentMode pmodes
  gqIx <- graphicsQueueFamilyIndex . vulkanDeviceBundle <$> getWindowBundle 
  pqIx <- presentQueueFamilyIndex . vulkanDeviceBundle <$> getWindowBundle 
  let whatImageSharingMode = if gqIx == pqIx then VK.SHARING_MODE_EXCLUSIVE else VK.SHARING_MODE_CONCURRENT
  let whatQueueFamilyIndices = if gqIx == pqIx then V.empty else V.fromList [gqIx, pqIx]
  return $
    VKSWAPCHAIN.SwapchainCreateInfoKHR
      () -- pNext
      VZ.zero -- swapchain create flags
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
      VKSURFACE.COMPOSITE_ALPHA_OPAQUE_BIT_KHR -- compositeAlpha
      presentingMode -- presentMode
      True -- clipped
      (maybe VK.NULL_HANDLE swapchainHandle previousSwapchain) -- old swapchain, if any


      
-- | Given some surface picks a suitable number of surfaces. This will:
--   - Try to use one more than the minimum number of images.
--   - If that exceeds the maximum allowed, uses the maximum allowed.
chooseSwapChainImageCount :: VKSURFACE.SurfaceCapabilitiesKHR -> Word32
chooseSwapChainImageCount s =
  let minImages = VKSURFACE.minImageCount s
      maxImages = VKSURFACE.maxImageCount s
      desiredImages = minImages + 1
  in -- if 'maxImages' is 0 there is no upper limit so we don't need to clamp
    if maxImages == 0
      then desiredImages
      else min desiredImages maxImages

-- | Picks a format. Tries to find an R8G8B8_UNORM format, but if that's
--   not found just picks the first format in the list.
choosePresentationFormat :: Vector VKSURFACE.SurfaceFormatKHR -> VKSURFACE.SurfaceFormatKHR
choosePresentationFormat fs =
  let desiredFormat = VK.FORMAT_R8G8B8A8_SRGB
      desiredColorspace = VKSURFACE.COLOR_SPACE_SRGB_NONLINEAR_KHR
      formatCount = length fs
      hasFormat =
        ( \f ->
                 (VKSURFACE.format f     == desiredFormat)
              && (VKSURFACE.colorSpace f == desiredColorspace)
        )
   in -- the driver can throw up it's hands (if it had hands) and say "idc what format you use"
      -- so we check for that first
      if formatCount == 1 && (VKSURFACE.format (fs ! 0) == VK.FORMAT_UNDEFINED)
        then VKSURFACE.SurfaceFormatKHR desiredFormat desiredColorspace
        else case find hasFormat fs of
          -- if we found a good format use it, otherwise settle for the first format
          Just f' -> f'
          Nothing -> fs ! 0

chooseSwapChainImageSize :: VKSURFACE.SurfaceCapabilitiesKHR -> Extent2D
chooseSwapChainImageSize s =
  let (Extent2D w _h) = VKSURFACE.currentExtent s
    in -- use whatever currentExtent is. If currentExtent is -1 we have to choose our own extent
      if w /= maxBound
        then VKSURFACE.currentExtent s
        else
          let (Extent2D minW minH) = VKSURFACE.minImageExtent s
              (Extent2D maxW maxH) = VKSURFACE.maxImageExtent s
              chooseW = max minW (min maxW 640)
              chooseH = max minH (min maxH 480)
          in Extent2D chooseW chooseH

checkSwapChainUsageAndTransform :: VKSURFACE.SurfaceCapabilitiesKHR -> (VK.ImageUsageFlagBits, VKSURFACE.SurfaceTransformFlagBitsKHR)
checkSwapChainUsageAndTransform s =
  let hasBits = VKSURFACE.supportedUsageFlags s
      needBits =
        [ VK.IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
          VK.IMAGE_USAGE_TRANSFER_DST_BIT
        ]
      checkBit = \bitX -> if hasBits .&. bitX == zeroBits then ["usage not supported: " ++ show bitX] else []
      checks = concatMap checkBit needBits
      allBits = foldl1 (.|.) needBits
   in --transformBits = supportedTransforms s
      if length checks > 0
        then error $ "Cannot create swapchain. " ++ concat checks
        else (allBits, VKSWAPCHAIN.SURFACE_TRANSFORM_IDENTITY_BIT_KHR)

choosePresentMode :: Vector VKSURFACE.PresentModeKHR -> VKSURFACE.PresentModeKHR
choosePresentMode pmodes =
  -- use fifo mode if found, otherwise error
  case find (== VKSURFACE.PRESENT_MODE_FIFO_KHR) pmodes of
    Nothing -> error "No FIFO presentation mode found"
    Just m -> m
