{-# LANGUAGE DataKinds #-}
module Fomorian.Vulkan.SwapchainBundle where

import Data.Vector (Vector(..))

import Vulkan.Core10 (Device, Format, Image, ImageView, Framebuffer, CommandPool, CommandBuffer, RenderPass)
import qualified  Vulkan.Core10 as VK
import qualified Vulkan.Core10.MemoryManagement as VKMEM
import qualified Vulkan.Zero as VZ
import qualified Vulkan.Extensions.VK_KHR_surface as VKSURFACE
import qualified Vulkan.Extensions.VK_KHR_swapchain ()
import Vulkan.Extensions (SwapchainKHR, SwapchainCreateInfoKHR)

import Fomorian.Vulkan.WindowBundle
import Fomorian.Vulkan.Resources.ImageBuffers

-- | Each image of the swapchain has a bunch of other related data: the ImageView, a Framebuffer, and maybe others?
data SwapchainPerImageData = SwapchainPerImageData Image ImageView Framebuffer
  deriving (Eq,Show)

data SwapchainBundle =
  SwapchainBundle
  {
    relevantCreateInfo :: SwapchainCreateInfoKHR '[],
    swapchainHandle :: SwapchainKHR,
    swapchainImages :: Vector SwapchainPerImageData
  }
  deriving (Show)

makeSwapchainBundle :: (InVulkanMonad effs) => RenderPass -> Eff effs SwapchainBundle
makeSwapchainBundle = do
  ci <- makeSwapChainCreateInfo s


destroySwapchainBundle :: SwapchainBundle -> IO ()
destroySwapchainBundle = undefined

makePerImageData :: Device -> Extent2D -> Image -> Format -> IO SwapchainPerImageData
makePerImageData d (Extent2D w h) img fmt =
  let idSw = VK.COMPONENT_SWIZZLE_IDENTITY
      viewInfo = VK.ImageViewCreateInfo () VZ.zero img VK.IMAGE_VIEW_TYPE_2D fmt (VK.ComponentMapping idSw idSw idSw idSw) (VK.ImageSubresourceRange VK.IMAGE_ASPECT_COLOR_BIT 0 1 0 1)
  in do
    iv <- VK.createImageView d viewInfo Nothing
    depthBuf <- makeDepthBuffer (w,h) VK.SAMPLE_COUNT_1_BIT Nothing
    return (SwapchainPerImageData img iv depthBuf)

destroyPerImageData :: SwapchainPerImageData -> IO ()
destroyPerImageData = undefined



-- | Get all the info for a given surface: capabilities, formats, and present modes.
--   Returns a swapchain create info struct with a NULL oldswapchain value.
makeSwapChainCreateInfo :: (InVulkanMonad effs) => Eff effs (SwapchainCreateInfoKHR '[])
makeSwapChainCreateInfo = do
  d <- getDevice
  s <- vulkanSurface <$> getWindowBundle
  capabilities <- getPhysicalDeviceSurfaceCapabilitiesKHR d s
  (SUCCESS, formats) <- getPhysicalDeviceSurfaceFormatsKHR d s
  (SUCCESS, pmodes) <- getPhysicalDeviceSurfacePresentModesKHR d s
  let desiredSwapChainImages = chooseSwapChainImageCount capabilities
  let presentFormat = choosePresentationFormat formats
  let swapChainSize = chooseSwapChainImageSize capabilities
  let (usageBits, transformBits) = checkSwapChainUsageAndTransform capabilities
  let presentingMode = choosePresentMode pmodes
  let whatImageSharingMode = if gq == pq then VK.SHARING_MODE_EXCLUSIVE else VK.SHARING_MODE_CONCURRENT
  let whatQueueFamilyIndices = if gq == pq then empty else V.fromList [gq, pq]
  return $
    SwapchainCreateInfoKHR
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
      VK.COMPOSITE_ALPHA_OPAQUE_BIT_KHR -- compositeAlpha
      presentingMode -- presentMode
      True -- clipped
      VK.NULL_HANDLE


      
-- | Given some surface picks a suitable number of surfaces. This will:
--   - Try to use one more than the minimum number of images.
--   - If that exceeds the maximum allowed, uses the maximum allowed.
chooseSwapChainImageCount :: SurfaceCapabilitiesKHR -> Word32
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
      if formatCount == 1 && (VKSURFACE.format (fs ! 0) == FORMAT_UNDEFINED)
        then SurfaceFormatKHR desiredFormat desiredColorspace
        else case find hasFormat fs of
          -- if we found a good format use it, otherwise settle for the first format
          Just f' -> f'
          Nothing -> fs ! 0

chooseSwapChainImageSize :: SurfaceCapabilitiesKHR -> Extent2D
chooseSwapChainImageSize s =
  let (Extent2D w _h) = currentExtent s
   in -- use whatever currentExtent is. If currentExtent is -1 we have to choose our own extent
      if w /= maxBound
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
