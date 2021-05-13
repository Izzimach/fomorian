{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Fomorian.Vulkan.WindowEtc
  ( VulkanConfig (..),
    WindowEtc (..),
    withWindowEtc,
    defaultInstanceConfig,
    validatedInstance,
    rebuildSwapChain
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Bits
import Data.ByteString (ByteString)
import Data.IORef
import Data.Vector ((!), Vector, empty, findIndex, fromList)
import Data.Word (Word32, Word64)
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import GHC.Int
import qualified Graphics.UI.GLFW as GLFW
import Vulkan.CStruct.Extends
import Vulkan.Core10 as VKCORE
import Vulkan.Core10.DeviceInitialization as VKDI
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_EXT_validation_features
import Vulkan.Extensions.VK_KHR_surface as VKSURFACE
import Vulkan.Extensions.VK_KHR_swapchain as VKSWAPCHAIN
import Vulkan.Extensions.VK_KHR_win32_surface
import Vulkan.Zero

import Fomorian.Windowing
import Fomorian.Vulkan.SwapChainEtc
import Fomorian.Vulkan.TransientResources

data WindowEtc = WindowEtc
  { vkInstance :: Instance,
    windowHandle :: GLFW.Window,
    vkDevice :: Device,
    vkChosen :: DeviceEtc,
    surfaceRef :: SurfaceKHR,
    graphicsQueue :: Queue,
    presentQueue :: Queue,
    imageAvailableSemaphores :: Vector Semaphore,
    renderingFinishedSemaphores :: Vector Semaphore,
    fences :: Vector Fence,
    cmdPool :: CommandPool,
    transients :: TransientResources,
    swapChainRef :: IORef SwapChainEtc
  }

data VulkanConfig a = VulkanConfig
  { createInfo :: InstanceCreateInfo a,
    inFlightFrames :: Int
  }

-- | This is a C function used as a callback to handle the vulkan validation messages.
--   Note that since vulkan calls are unsafe by default you can't write a debug callback in Haskell.
--   To change this you can compile the vulkan package with 'safe-foreign-calls'.
foreign import ccall unsafe "VulkanCallback.c &vulkanDebugCallback" debugCallbackPtr :: PFN_vkDebugUtilsMessengerCallbackEXT

data VulkanValidation = UseValidation | NoValidation
  deriving (Eq, Show)

-- | Setup for creating a Vulkan instance. Edit fields here to change your application name or version to use.
--   Notably this config enables surface extensions so that the GLFW function 'createWindowSurface' will work.
defaultInstanceConfig :: InstanceCreateInfo '[]
defaultInstanceConfig =
  InstanceCreateInfo
    () -- pNext
    zero -- flags
    ( Just $
        ApplicationInfo
          (Just ("FomorianTest" :: ByteString)) -- application name
          1 -- application version
          (Just ("fomorian" :: ByteString)) -- engine name
          1 -- engine version
          ((shift 1 22) .|. (shift 0 12) .|. (shift 0 0)) -- major/minor/patch numbers, packed
    )
    empty -- enabledLayerNames
    (fromList [KHR_SURFACE_EXTENSION_NAME, KHR_WIN32_SURFACE_EXTENSION_NAME]) -- enabledExtensionNames

-- | Pass in an InstanceCreateInfo and this function will modify it to add validation layers.
validatedInstance :: InstanceCreateInfo '[] -> InstanceCreateInfo '[DebugUtilsMessengerCreateInfoEXT, ValidationFeaturesEXT]
validatedInstance baseCreateInfo =
  let debugCreateInfo =
        DebugUtilsMessengerCreateInfoEXT
          zero
          (DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT)
          (DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT)
          debugCallbackPtr
          nullPtr
      validationFeatures = ValidationFeaturesEXT (fromList [VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT]) empty
      debugLayers =
        (VKDI.enabledLayerNames baseCreateInfo)
          <> (fromList ["VK_LAYER_KHRONOS_validation"])
      debugExtensions =
        (VKDI.enabledExtensionNames baseCreateInfo)
          <> (fromList [EXT_VALIDATION_FEATURES_EXTENSION_NAME, EXT_DEBUG_UTILS_EXTENSION_NAME])
   in -- modify the layers and extensions fields
      ( baseCreateInfo
          { VKDI.enabledLayerNames = debugLayers,
            VKDI.enabledExtensionNames = debugExtensions
          }
      )
        -- add some extra information structs to the pNext chain
        ::& (debugCreateInfo :& validationFeatures :& ())

-- | Vulkan initialization boilerplate. Goes through several levels of allocation and
--   bracketing to create and acquire several vulkan resources:
--   - The GLFW window
--   - The Vulkan instance
--   - a SurfaceKHR for the window
--   - a vulkan device that works with the surface
--   - a graphics and presentation queue
--   - two semaphores, to signal events to the swapchain
--
--   All these are wrapped with the user-provided wrapper function (which is usually just 'bracket')
--   to deallocate resources when the program exits, with or without an exception.
withWindowEtc ::
  (PokeChain q, Extendss InstanceCreateInfo q) =>
  VulkanConfig q ->
  WindowInitData ->
  Maybe AllocationCallbacks ->
  (forall a b c. IO a -> (a -> IO b) -> (a -> IO c) -> IO c) ->
  (WindowEtc -> IO ()) ->
  IO ()
withWindowEtc vCfg wid allocator wrapper wrapped = wrapper startWindow endWindow goWindow
  where
    simpleSemaphoreConfig = (SemaphoreCreateInfo () zero)
    simpleFenceConfig = (FenceCreateInfo () FENCE_CREATE_SIGNALED_BIT)
    startWindow = initWindow wid
    endWindow w = terminateWindow w
    goWindow w =
      withInstance (createInfo vCfg) allocator wrapper $ \inst -> do
        withSurface inst w wrapper $ \surfaceH -> do
          choice <- runMaybeT $ chooseVulkanDevice inst surfaceH
          case choice of
            Nothing -> return ()
            Just chosen@(DeviceEtc h gq pq) -> do
              withDevice h (chosenDeviceCreateInfo chosen) allocator wrapper $ \device -> do
                graphicsQ <- getDeviceQueue device gq 0
                presentQ <- getDeviceQueue device pq 0
                withSyncObjects device simpleSemaphoreConfig simpleFenceConfig Nothing (inFlightFrames vCfg) wrapper $ \syncs -> do
                  let commandPoolInfo = CommandPoolCreateInfo zero gq
                  withCommandPool device commandPoolInfo Nothing wrapper $ \cmdpool -> do
                    withTransientResources device h cmdpool graphicsQ Nothing wrapper $ \tRes -> do
                      swapchainInfo <- generateSwapChainInfo chosen surfaceH
                      withSwapChainEtc device h cmdpool tRes graphicsQ swapchainInfo Nothing wrapper $ \swapchainEtc -> do
                        let (SyncObjects imageAvailables renderingFinisheds localFences) = syncs
                        wrapped (WindowEtc inst w device chosen surfaceH graphicsQ presentQ imageAvailables renderingFinisheds localFences cmdpool tRes swapchainEtc)

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

-- | Picks a suitable vulkan device using 'suitableDevice' and finds the right queue family/families
--   for graphics and presentation.
chooseVulkanDevice :: (MonadPlus m, MonadIO m) => Instance -> SurfaceKHR -> m DeviceEtc
chooseVulkanDevice i s = do
  d <- findSuitableDevice i s
  gq <- findGraphicsQueue d
  pq <- findSurfaceQueue s d
  return (DeviceEtc d gq pq)

-- | Picks a suitable device, using the criteria of 'suitableDevice' below.
findSuitableDevice :: (MonadIO m, MonadPlus m) => Instance -> SurfaceKHR -> m PhysicalDevice
findSuitableDevice i s = do
  (_, devices) <- liftIO $ enumeratePhysicalDevices i
  vBools <- liftIO $ mapM (suitableDevice s) devices
  case (findIndex id vBools) of
    Nothing -> mzero -- none found
    Just ix -> return $ (devices ! ix)

-- | Given a device, returns true if this device is suitable, where suitable means:
--    - Has a queue that supports graphics.
--    - Supports the surface provided.
suitableDevice :: SurfaceKHR -> PhysicalDevice -> IO Bool
suitableDevice s d = do
  features <- getPhysicalDeviceFeatures d
  let supportsAnisotropy = samplerAnisotropy features 
  _properties <- getPhysicalDeviceProperties d
  _mem <- getPhysicalDeviceMemoryProperties d
  gq <- runMaybeT $ findGraphicsQueue d
  pq <- runMaybeT $ findSurfaceQueue s d
  -- we need both a graphics queue and presentation queue to be valid
  case (gq, pq) of
    (Just _, Just _) -> return supportsAnisotropy
    (_, _) -> return False

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
       in -- do a bitwise AND to check if this supports graphics
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
        else do
          l <- liftIO $ getPhysicalDeviceSurfaceSupportKHR d ix s
          if l
            then return ix
            else checkQueueIndex (ix + 1) count s d

chosenDeviceCreateInfo :: DeviceEtc -> DeviceCreateInfo '[]
chosenDeviceCreateInfo (DeviceEtc _h gq pq) =
  let simpleQueue qx = SomeStruct $ DeviceQueueCreateInfo () (DeviceQueueCreateFlagBits 0) qx (fromList [1.0])
      queueList =
        if (gq == pq)
          then [simpleQueue gq]
          else [simpleQueue gq, simpleQueue pq]
      deviceFeatures = zero { samplerAnisotropy = True }
   in DeviceCreateInfo
        ()
        (DeviceCreateFlags 0)
        (fromList queueList) -- queues
        (fromList []) -- enabledLayerNames
        (fromList [KHR_SWAPCHAIN_EXTENSION_NAME]) -- enabledExtensionNames
        (Just deviceFeatures) -- device features to enable

data SyncObjects = SyncObjects (Vector Semaphore) (Vector Semaphore) (Vector Fence)
  deriving (Show)

withSyncObjects ::
  (MonadIO io) =>
  Device ->
  SemaphoreCreateInfo '[] ->
  FenceCreateInfo '[] ->
  Maybe AllocationCallbacks ->
  Int ->
  (io SyncObjects -> (SyncObjects -> io ()) -> r) ->
  r
withSyncObjects device semcreate fencecreate alloc count wrapper = wrapper startSyncObjects endSyncObjects
  where
    startSyncObjects :: (MonadIO io) => io SyncObjects
    startSyncObjects = do
      let counts = [1 .. count]
      isems <- mapM (\_ -> createSemaphore device semcreate alloc) counts
      rsems <- mapM (\_ -> createSemaphore device semcreate alloc) counts
      localFences <- mapM (\_ -> createFence device fencecreate alloc) counts
      return $ SyncObjects (fromList isems) (fromList rsems) (fromList localFences)
    endSyncObjects :: (MonadIO io) => SyncObjects -> io ()
    endSyncObjects (SyncObjects isems rsems localFences) = do
      mapM_ (\s -> destroySemaphore device s alloc) isems
      mapM_ (\s -> destroySemaphore device s alloc) rsems
      mapM_ (\f -> destroyFence device f alloc) localFences

rebuildSwapChain :: WindowEtc -> Maybe AllocationCallbacks -> IO ()
rebuildSwapChain windowEtc allocator = 
  let device = vkDevice windowEtc
      chosen = vkChosen windowEtc
      phy = physicalHandle chosen
      cpool = (cmdPool windowEtc)
      tRes = transients windowEtc
  in
    do
      swapchainEtc <- readIORef (swapChainRef windowEtc)
      newswapchain <- recreateSwapChainEtc device phy cpool tRes swapchainEtc chosen (surfaceRef windowEtc) allocator
      liftIO $ writeIORef (swapChainRef windowEtc) newswapchain

