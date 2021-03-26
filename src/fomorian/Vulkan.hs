{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Fomorian.Vulkan where

import Data.ByteString (ByteString)
import Data.Foldable
import Data.IORef
import Data.Word (Word32, Word64)
import Data.Bits
import Data.Vector (Vector(..), fromList, findIndex, (!), empty)

import GHC.Int

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_KHR_surface as VKSURFACE
import Vulkan.Extensions.VK_KHR_win32_surface
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_EXT_validation_features

import qualified Graphics.UI.GLFW as GLFW

import Fomorian.Windowing

foreign import ccall unsafe "SomeCode.c &debugCallback"
  debugCallbackPtr :: PFN_vkDebugUtilsMessengerCallbackEXT

-- | Setup for creating a Vulkan instance. Edit fields here to change your application name or version to use.
--   Notably this config enables surface extensions so that the GLFW function 'createWindowSurface' will work.
instanceConfig :: FunPtr FN_vkDebugUtilsMessengerCallbackEXT -> InstanceCreateInfo '[DebugUtilsMessengerCreateInfoEXT, ValidationFeaturesEXT]
instanceConfig vulkanDebugCallback =
  -- debug to log warning and errors
  let debugCreateInfo = DebugUtilsMessengerCreateInfoEXT zero
        (DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT)
        (DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT)
        vulkanDebugCallback
        nullPtr
      validationFeatures = ValidationFeaturesEXT (fromList [VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT]) empty
      createInfo = InstanceCreateInfo
        ()   -- pNext
        zero -- flags
        (Just $
          ApplicationInfo
            (Just ("FomorianTest" :: ByteString)) -- application name
            1                     -- application version
            (Just ("fomorian" :: ByteString))     -- engine name
            1                     -- engine version
            ((shift 1 22) .|. (shift 0 12) .|. (shift 0 0)) -- major/minor/patch numbers, packed
        )
        (fromList [ "VK_LAYER_KHRONOS_validation" ]) -- enabledLayerNames
        (fromList [
          KHR_SURFACE_EXTENSION_NAME,
          KHR_WIN32_SURFACE_EXTENSION_NAME,
          EXT_VALIDATION_FEATURES_EXTENSION_NAME,
          EXT_DEBUG_UTILS_EXTENSION_NAME
          ])  -- extensionNames
  in
    createInfo ::& debugCreateInfo :& validationFeatures :& ()




main :: IO ()
main = do
    withInstance (instanceConfig debugCallbackPtr) Nothing bracket $ \i -> do
      --debugInstance i
      let windowConfig = WindowInitData 600 400 "Vulkan test window" NoOpenGL
      withWindowEtc i windowConfig bracket $ \(WindowEtc inst h d surf gq pq is rs swref) -> do
        putStrLn $ show is
        putStrLn $ show rs
        swapchainEtc <- readIORef swref
        putStrLn $ show swapchainEtc


data WindowEtc =
  WindowEtc {
    vkInstance :: Instance,
    windowHandle :: GLFW.Window,
    vkDevice :: Device,
    surfaceRef :: SurfaceKHR,
    graphicsQueue :: Queue,
    presentQueue :: Queue,
    imageAvailableSemaphore :: Semaphore,
    renderingFinishedSemaphore :: Semaphore,
    swapChainRef :: IORef SwapchainEtc
  }

-- | Holds a swapchain and the associated data handles. 
data SwapchainEtc =
  SwapchainEtc {
    theSwapchain :: SwapchainKHR,
    swapchainImages :: Vector Image,
    swapchainCreateInfo :: SwapchainCreateInfoKHR '[]
  }
  deriving (Show)

-- | Vulkan initialization boilerplate. Goes through several levels of allocation and
--   bracketing to create and acquire several vulkan resources:
--   - The GLFW window
--   - a SurfaceKHR for that window
--   - a vulkan device that works with the surface
--   - a graphics and presentation queue
--   - two semaphores, to signal events to the swapchain
--
--   All these are wrapped with the user-provided wrapper function (which is usually just 'bracket')
--   to deallocate resources when the program exits, with or without an exception.
--
withWindowEtc :: Instance -> WindowInitData -> (forall a b c. IO a -> (a -> IO b) -> (a -> IO c) -> IO c) -> (WindowEtc -> IO ()) -> IO ()
withWindowEtc inst wid wrapper wrapped = wrapper startWindow endWindow goWindow
  where
    simpleSemaphoreConfig = (SemaphoreCreateInfo () (SemaphoreCreateFlags 0))

    startWindow = initWindow wid

    endWindow w = terminateWindow w

    goWindow w = withSurface inst w wrapper $ \surfaceH -> do
      choice <- runMaybeT $ chooseVulkanDevice inst surfaceH
      case choice of
        Nothing -> return ()
        Just c@(ChosenDevice h gq pq) -> do
          withDevice h (chosenDeviceCreateInfo c) Nothing wrapper $ \device -> do
            graphicsQ <- getDeviceQueue device gq 0
            presentQ <- getDeviceQueue device pq 0
            withSemaphore device simpleSemaphoreConfig Nothing wrapper $ \imageAvailable ->
              withSemaphore device simpleSemaphoreConfig Nothing wrapper $ \renderingFinished -> do
                swapchainInfo <- generateSwapchainInfo c surfaceH
                withSwapchainEtc device swapchainInfo Nothing wrapper $ \swapchainEtc ->
                  wrapped (WindowEtc inst w device surfaceH graphicsQ presentQ imageAvailable renderingFinished swapchainEtc)


-- | Creates the vulkan surface via GLFW, and deallocates it at the end using a bracketing function.
withSurface :: Instance -> GLFW.Window -> (forall a b c. IO a -> (a -> IO b) -> (a -> IO c) -> IO c) -> (SurfaceKHR -> IO ()) -> IO ()
withSurface inst wid wrapper goSurface = wrapper startSurface endSurface goSurface
  where
    startSurface = do
      alloca @Word64 $ \pSurface -> do
        result <- (GLFW.createWindowSurface (instanceHandle inst) wid nullPtr pSurface :: IO GHC.Int.Int32)
        if (result < 0)
        then fail "Could not create window surface"
        else fmap SurfaceKHR $ peek @Word64 pSurface

    endSurface surf = destroySurfaceKHR inst surf Nothing


data ChosenDevice =
  ChosenDevice {
    deviceHandle :: PhysicalDevice,
    graphicsQueueIndex :: Word32,
    presentQueueIndex :: Word32
  }
  deriving (Eq, Show)

-- | Picks a suitable vulkan decide using 'suitableDevice' and finds the right queue family/families
--   for graphics and presentation.
chooseVulkanDevice :: (MonadPlus m, MonadIO m) => Instance -> SurfaceKHR -> m ChosenDevice
chooseVulkanDevice i s = do
  d <- findSuitableDevice i s
  gq <- findGraphicsQueue d
  pq <- findSurfaceQueue s d
  return (ChosenDevice d gq pq)


-- | Picks a suitable device, using the criteria of 'suitableDevice'.
findSuitableDevice :: (MonadIO m, MonadPlus m) => Instance -> SurfaceKHR -> m PhysicalDevice
findSuitableDevice i s = do
  (_, devices) <- liftIO $ enumeratePhysicalDevices i
  vBools <- liftIO $ mapM (suitableDevice s) devices
  case (findIndex id vBools) of
    Nothing -> mzero  -- none found
    Just ix -> return $ (devices ! ix)


-- | Given a device, returns true if this device is suitable, where suitable means:
--    - Has a queue that supports graphics.
--    - Supports the surface provided.
suitableDevice :: SurfaceKHR -> PhysicalDevice -> IO Bool
suitableDevice s d = do
  _features <- getPhysicalDeviceFeatures d
  _properties <- getPhysicalDeviceProperties d
  _mem <- getPhysicalDeviceMemoryProperties d
  gq <- runMaybeT $ findGraphicsQueue d
  pq <- runMaybeT $ findSurfaceQueue s d
  -- we need both a graphics queue and presentation queue to be valid
  case (gq,pq) of
    (Just _, Just _) -> return True
    (_,_)            -> return False


-- | Given a device, returns index of the first queue that supports
--   graphics ('QUEUE_GRAPHICS_BIT' is set).
findGraphicsQueue :: (MonadIO m, MonadPlus m) => PhysicalDevice -> m Word32
findGraphicsQueue d = 
  do
    queues <- getPhysicalDeviceQueueFamilyProperties d
    case (findIndex hasGraphics queues) of
      Nothing -> mzero
      Just q' -> return (fromIntegral q')
  where
    hasGraphics q =
      let (QueueFlagBits queueBits) = (queueFlags q)
          (QueueFlagBits graphicsBit) = QUEUE_GRAPHICS_BIT
      in
        -- do a bitwise AND to check if this supports graphics
        (queueBits .&. graphicsBit) > 0


-- | Given a surface and a device, return the index of the first queue family of the device that
--   supports the surface, or 'mzero' if there isn't one.
findSurfaceQueue :: (MonadPlus m, MonadIO m) => SurfaceKHR -> PhysicalDevice -> m Word32
findSurfaceQueue surfaceH device = do
  queueFamilyCount <- fmap length $ getPhysicalDeviceQueueFamilyProperties device
  checkQueueIndex 0 (fromIntegral queueFamilyCount) surfaceH device
    where
      checkQueueIndex ix count s d =
        if (ix >= count)
        then mzero
        else
          do
            l <- liftIO $ getPhysicalDeviceSurfaceSupportKHR d ix s
            if l
            then return ix
            else checkQueueIndex (ix+1) count s d

chosenDeviceCreateInfo :: ChosenDevice -> DeviceCreateInfo '[]
chosenDeviceCreateInfo (ChosenDevice h gq pq) =
  let simpleQueue x = SomeStruct $ DeviceQueueCreateInfo () (DeviceQueueCreateFlagBits 0) x (fromList [1.0])
      queueList = if (gq == pq)
                  then [simpleQueue gq]
                  else [simpleQueue gq, simpleQueue pq]
  in DeviceCreateInfo
              ()
              (DeviceCreateFlags 0)
              (fromList queueList)  -- queues
              (fromList [])  -- enabledLayerNames
              (fromList [KHR_SWAPCHAIN_EXTENSION_NAME])  -- enabledExtensionNames
              Nothing        -- device features to enable

-- | Get all the info for a given surface: capabilities, formats, present modes.
--   Returns a swapchain create info struct with a NULL oldswapchain value.
generateSwapchainInfo :: ChosenDevice -> SurfaceKHR -> IO (SwapchainCreateInfoKHR '[])
generateSwapchainInfo (ChosenDevice d gq pq) s = do
  capabilities <- getPhysicalDeviceSurfaceCapabilitiesKHR d s
  (SUCCESS,formats) <- getPhysicalDeviceSurfaceFormatsKHR d s
  (SUCCESS,modes) <- getPhysicalDeviceSurfacePresentModesKHR d s
  let desiredSwapChainImages = chooseSwapChainImageCount capabilities
  let presentFormat = choosePresentationFormat formats
  let swapChainSize = chooseSwapChainImageSize capabilities
  let (usageBits, transformBits) = checkSwapChainUsageAndTransform capabilities
  let presentingMode = choosePresentMode modes
  let imageSharingMode = if (gq == pq) then SHARING_MODE_EXCLUSIVE else SHARING_MODE_CONCURRENT
  let queueFamilyIndices = if (gq == pq) then empty else (fromList [gq,pq])
  return $ SwapchainCreateInfoKHR
             ()    -- pNext
             zero  -- swapchain create flags
             s     -- surface
             desiredSwapChainImages -- minImageCount
             (VKSURFACE.format presentFormat)     -- imageFormat
             (VKSURFACE.colorSpace presentFormat) -- imageColorSpace
             swapChainSize                        -- imageExtent
             1                                    -- imageArrayLayers
             usageBits                            -- imageUsage
             imageSharingMode                  -- imageSharingMode
             queueFamilyIndices                -- queueFamilyIndices if sharing is concurrent
             transformBits                     -- preTransform
             COMPOSITE_ALPHA_OPAQUE_BIT_KHR    -- compositeAlpha
             presentingMode                       -- presentMode
             True                              -- clipped
             NULL_HANDLE

-- | Creates a swapchain and passes and IORef to the wrapped user function. We provide an IORef because
--   sometimes you have to recreate the swapchain in-place without restarting the program.
withSwapchainEtc :: (MonadIO io) => Device -> SwapchainCreateInfoKHR '[] -> Maybe AllocationCallbacks -> (io (IORef SwapchainEtc) -> (IORef SwapchainEtc -> io ()) -> r) -> r
withSwapchainEtc d ci allocator wrapper = wrapper startSwapchainRef endSwapchainRef
  where
    startSwapchainRef :: (MonadIO io) => io (IORef SwapchainEtc)
    startSwapchainRef =
      do
        swapchain <- createSwapchainKHR d ci allocator
        (_,newImages) <- getSwapchainImagesKHR d swapchain
        liftIO $ newIORef (SwapchainEtc swapchain newImages ci)
    endSwapchainRef :: (MonadIO io) => IORef SwapchainEtc -> io ()
    endSwapchainRef ref =
      do
        (SwapchainEtc swapchain images ci) <- liftIO $ readIORef ref
        destroySwapchainKHR d swapchain allocator

prepareSwapchainEtc :: WindowEtc -> IO ()
prepareSwapchainEtc (WindowEtc inst h d surf gq pq is rs swref) = do
  swapchainStuff <- readIORef swref
  (SUCCESS,newImages) <- getSwapchainImagesKHR d (theSwapchain swapchainStuff)
  let newswapchainStuff = swapchainStuff { swapchainImages = newImages };
  writeIORef swref newswapchainStuff
  return ()






-- | Given some surface picks a suitable number of surfaces. This will:
--   - Try to use one more than the minimum number of images.
--   - If that exceeds the maximum allowed, uses the maximum allowed.
chooseSwapChainImageCount :: SurfaceCapabilitiesKHR -> Word32
chooseSwapChainImageCount s =
  let minImages = (VKSURFACE.minImageCount s)
      maxImages = (VKSURFACE.maxImageCount s)
      desiredImages = minImages + 1
  in
    -- if 'maxImages' is 0 there is no upper limit so we don't need to clamp
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
      hasFormat = (\f -> (VKSURFACE.format f == desiredFormat) &&
                         (VKSURFACE.colorSpace f == desiredColorspace))
  in
    -- the driver can throw up it's hands (if it had hands) and say "idc what format you use"
    -- so we check for that first
    if (formatCount == 1 && ((VKSURFACE.format (fs ! 0)) == FORMAT_UNDEFINED))
    then
      (SurfaceFormatKHR desiredFormat desiredColorspace)
    else 
      case (find hasFormat fs) of
        -- if we found a good format use it, otherwise settle for the first format
        Just f' -> f'
        Nothing -> fs ! 0

chooseSwapChainImageSize :: SurfaceCapabilitiesKHR -> Extent2D
chooseSwapChainImageSize s =
  let (Extent2D w h) = currentExtent s
  in
    -- use whatever currentExtent is. If currentExtent is -1 we have to choose our own extent
    if (w /= maxBound)
    then currentExtent s
    else let (Extent2D minW minH) = minImageExtent s
             (Extent2D maxW maxH) = maxImageExtent s
             chooseW = max minW (min maxW 640)
             chooseH = max minH (min maxH 480)
         in (Extent2D chooseW chooseH)

checkSwapChainUsageAndTransform :: SurfaceCapabilitiesKHR -> (ImageUsageFlagBits, SurfaceTransformFlagBitsKHR)
checkSwapChainUsageAndTransform s =
  let hasBits = supportedUsageFlags s
      needBits = [IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
                  IMAGE_USAGE_TRANSFER_DST_BIT]
      checkBit = \bit -> if (hasBits .&. bit) == zero then ["usage not supported: " ++ show bit] else []
      checks = concat $ map checkBit needBits
      allBits = foldl1 (.|.) needBits
      transformBits = supportedTransforms s
  in
    if (length checks > 0)
    then error $ "Cannot create swapchain. " ++ (concat checks)
    else (allBits, SURFACE_TRANSFORM_IDENTITY_BIT_KHR)

choosePresentMode :: Vector PresentModeKHR -> PresentModeKHR
choosePresentMode pmodes =
  -- use fifo mode if found, otherwise error
  case (find (== PRESENT_MODE_FIFO_KHR) pmodes) of
    Nothing -> error "No FIFO presentation mode found"
    Just m -> m

--
-- extra functions to dump vulkan debug text.
--

debugInstance :: Instance -> IO ()
debugInstance i = do
  (_, layerz) <- liftIO enumerateInstanceLayerProperties
  (_, extensionz) <- enumerateInstanceExtensionProperties Nothing
  putStrLn (show extensionz)
  --putStrLn (show layerz)
  (_, devices) <- enumeratePhysicalDevices i
  traverse_ deviceInfo devices


deviceInfo :: (MonadIO m) => PhysicalDevice -> m ()
deviceInfo p = do
  (_, extensions) <- enumerateDeviceExtensionProperties p Nothing
  (_, layers    ) <- enumerateDeviceLayerProperties p
  liftIO $ traverse_ myPrint extensions
  liftIO $ traverse_ myPrint layers
  --myPrint =<< getPhysicalDeviceFeatures p
  myPrint =<< getPhysicalDeviceQueueFamilyProperties p
  myPrint =<< getPhysicalDeviceProperties p
  --myPrint =<< getPhysicalDeviceMemoryProperties p
  where
    myPrint :: (MonadIO m, Show a) => a -> m ()
    myPrint = liftIO . putStrLn . show

