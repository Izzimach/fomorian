{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | WindowBundle opens a window and does a lot of the vulkan boilerplate initialization, with
--   the common choices made so you don't have to make them. Choosing devices, surfaces, queues, etc.
--   are all done with (hopefully) reasonable default settings.
module Fomorian.Vulkan.WindowBundle where

import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Cont

import Data.Bits
import Data.ByteString (ByteString, packCString)
import Data.Foldable (find)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.IORef
import Data.Vector ((!), Vector, empty, findIndex, fromList, toList)
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
import Vulkan.Zero

import Fomorian.Windowing

-- | This is a C function used as a callback to handle the vulkan validation messages.
--   Note that since vulkan calls are unsafe by default you can't write a debug callback in Haskell.
--   To change this you can compile the vulkan package with 'safe-foreign-calls'.
foreign import ccall unsafe "VulkanCallback.c &vulkanDebugCallback" debugCallbackPtr :: PFN_vkDebugUtilsMessengerCallbackEXT



-- | Config of window/vulkan initialization, using mostly user-friendly fields
data WindowInitConfig = WindowInitConfig {
    -- | name as utf8 text
    appName :: Text,
    -- | title as utf8 text
    windowTitle :: Text,
    -- | Allocator callbacks, if any
    userAllocator :: Maybe AllocationCallbacks,      
    -- | If true, then init will enable validation layers and add a callback that prints errors to stdout
    enableDebugLayer :: Bool
  }
  deriving (Show)

-- | The 'WindowBundle' holds most of the data that gets created in Vulkan boilerplate, such as the instance, device, surface, etc.
data WindowBundle = WindowBundle {
    vulkanInstance :: Instance,
    windowHandle :: GLFW.Window,
    vulkanSurface :: SurfaceKHR,
    vulkanDeviceBundle :: DeviceBundle
    --swapChainRef :: IORef SwapChainEtc
  }

-- | The device bundle holds both the vulkan device handle and the various queues that were requested.
data DeviceBundle = DeviceBundle
  {
    deviceHandle :: Device,
    physicalDeviceHandle :: PhysicalDevice,
    -- | Queue to use for graphics drawing commands
    graphicsQueue :: Queue,
    -- | Queue to use for presenting the buffer to a surface
    presentQueue :: Queue,
    -- | Extra lower-priority queues, usually used by the loader for transfer of resources between host and device memory.
    auxiliaryQueues :: [Queue]
  }
  deriving (Eq, Show)


withWindowBundle :: WindowInitConfig -> (WindowBundle -> IO ()) -> IO ()
withWindowBundle config wrapped = bracket startBundle endBundle goBundle
  where
    startBundle = initWindow (WindowInitData 600 400 "argh" NoOpenGL)
    endBundle = terminateWindow
    goBundle window = do
      glfwExtensions <- getGLFWExtensions
      let createInfo = makeInstanceCreateInfo config glfwExtensions
      let allocator = userAllocator config
      withInstance createInfo allocator bracket $ \vkInstance -> do
        withSurface vkInstance window $ \vkSurface -> do
          withPickAndMakeDevice vkInstance vkSurface 0 allocator $ \deviceBundle -> do
            wrapped (WindowBundle vkInstance window vkSurface deviceBundle)

  


-- | Given a config, create an InstanceCreateInfo. Debug and validation structs are always there because of how
--   struct typing in the vulkan haskell package works; adding or removing debug structs changes the type of the InstanceCreateInfo,
--   which means the return type depends on whether debug is enabled -- a dependent type.  Instead of using dependent types here I decide to
--   always create a debug and validation struct and just leaving them empty if debug is not enabled.
--   Also takes a list of additional extensions to support, used because GLFW needs to have one (or more) surface-relevant extensions enabled.
makeInstanceCreateInfo :: WindowInitConfig -> [ByteString] -> InstanceCreateInfo '[DebugUtilsMessengerCreateInfoEXT, ValidationFeaturesEXT]
makeInstanceCreateInfo config baseExtensions =
  let debugExtensionStruct =
        DebugUtilsMessengerCreateInfoEXT
          zero
          (DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT)
          (DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT)
          debugCallbackPtr
          nullPtr

      validationFeaturesStruct =
        ValidationFeaturesEXT (fromList [VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT]) empty
      
      -- should we use debugging aids?
      useDebug = (enableDebugLayer config)
      useLayers = if (useDebug) 
                  then ["VK_LAYER_KHRONOS_validation"]
                  else mempty
      useExtensions = if (useDebug)
                      then baseExtensions <> [EXT_VALIDATION_FEATURES_EXTENSION_NAME, EXT_DEBUG_UTILS_EXTENSION_NAME]
                      else baseExtensions
  in
    InstanceCreateInfo
      () -- pNext
      zero -- flags
      ( Just $
          ApplicationInfo
            (Just (encodeUtf8 (appName config))) -- application name
            1 -- application version
            (Just ("fomorian" :: ByteString)) -- engine name
            1 -- engine version
            ((shift 1 22) .|. (shift 0 12) .|. (shift 0 0)) -- major/minor/patch numbers, packed
      )
      (fromList useLayers) -- enabledLayerNames
      (fromList useExtensions) -- enabledExtensionNames

        -- add some extra information structs to the pNext chain
        ::& (debugExtensionStruct :& validationFeaturesStruct :& ())

-- | Asks GLFW which extensions it requries. These are returned from GLFW as CStrings so they are converted into ByteStrings for use with the vulkan package.
getGLFWExtensions :: IO [ByteString]
getGLFWExtensions = GLFW.getRequiredInstanceExtensions >>= mapM packCString


-- | Creates the vulkan surface via GLFW, and deallocates it at the end using a bracketing function.
withSurface :: Instance -> GLFW.Window -> (SurfaceKHR -> IO ()) -> IO ()
withSurface inst wid goSurface = bracket startSurface endSurface goSurface
  where
    startSurface = do
      alloca @Word64 $ \pSurface -> do
        result <- (GLFW.createWindowSurface (instanceHandle inst) wid nullPtr pSurface :: IO GHC.Int.Int32)
        if (result < 0)
          then fail "Could not create window surface"
          else fmap SurfaceKHR $ peek @Word64 pSurface
    endSurface surf = destroySurfaceKHR inst surf Nothing

data DeviceAndQueues = DeviceAndQueues 
  {
    physicalHandle :: PhysicalDevice,
    graphicsQueueFamilyIndex :: Word32,
    presentQueueFamilyIndex :: Word32
  }
  deriving (Eq, Show)

withPickAndMakeDevice :: Instance -> SurfaceKHR -> Word32 -> Maybe AllocationCallbacks ->  (DeviceBundle -> IO ()) -> IO ()
withPickAndMakeDevice vkInstance vkSurface auxQueueCount allocator wrapped = do
  deviceConfig <- pickDeviceAndQueues vkInstance vkSurface
  case deviceConfig of
    Nothing -> error "No valid vulkan device found that supports a present queue for this surface."
    Just dq -> do
      let deviceCreateInfo = makeDeviceCreateInfo dq 0
      let physDevice = physicalHandle dq
      withDevice physDevice deviceCreateInfo allocator bracket $ \vkDevice -> do
        graphicsQ <- getDeviceQueue vkDevice (graphicsQueueFamilyIndex dq) 0
        presentQ <- getDeviceQueue vkDevice (presentQueueFamilyIndex dq) 0
        auxQs <- mapM (\ix -> getDeviceQueue vkDevice (graphicsQueueFamilyIndex dq) ix) [1..auxQueueCount]
        wrapped (DeviceBundle vkDevice physDevice graphicsQ presentQ auxQs)

-- | pick a device that has both a graphics queue and a present queue for the given instance and surface
pickDeviceAndQueues :: Instance -> SurfaceKHR -> IO (Maybe DeviceAndQueues)
pickDeviceAndQueues inst surf = do
  (_, devices) <- liftIO $ enumeratePhysicalDevices inst
  checkDevices (toList devices)
  where
    checkDevices [] = return Nothing
    checkDevices (d : ds) = do deviceVal <- validDevice d
                               case deviceVal of
                                 Nothing -> checkDevices ds
                                 Just _ -> return deviceVal
    
    validDevice d = do
      gq <- findGraphicsQueueForDevice d
      pq <- findPresentQueueForDevice d surf
      case (gq,pq) of
        (Just gq', Just pq') -> return $ Just (DeviceAndQueues d gq' pq')
        _                    -> return Nothing

-- | Given a device, returns index of the first queue family that supports
--   graphics ('QUEUE_GRAPHICS_BIT' is set).
findGraphicsQueueForDevice :: PhysicalDevice -> IO (Maybe Word32)
findGraphicsQueueForDevice d =
  do
    queues <- getPhysicalDeviceQueueFamilyProperties d
    return $ fmap fromIntegral (findIndex hasGraphics queues)
  where
    hasGraphics q =
      let (QueueFlagBits queueBits) = (queueFlags q)
          (QueueFlagBits graphicsBit) = QUEUE_GRAPHICS_BIT
       in -- do a bitwise AND to check if this supports graphics
          (queueBits .&. graphicsBit) > 0

-- | Given a surface and a device, return the index of the first queue family of the device that
--   supports the surface, or 'mzero' if there isn't one.
findPresentQueueForDevice ::PhysicalDevice -> SurfaceKHR -> IO (Maybe Word32)
findPresentQueueForDevice device surf = do
  queueFamilyCount <- fmap length $ getPhysicalDeviceQueueFamilyProperties device
  checkQueueIndex 0 (fromIntegral queueFamilyCount) surf device
  where
    checkQueueIndex ix count s d =
      if (ix >= count)
        then return Nothing
        else do
          l <- liftIO $ getPhysicalDeviceSurfaceSupportKHR d ix s
          if l
            then return (Just ix)
            else checkQueueIndex (ix + 1) count s d

-- | Create a DeviceCreateInfo given the DeviceAndQueues information. Sets up the device to create:
--   - two default queues, a graphics queue and present queue
--   - zero or more auxiliary queues in the same family as the graphics queue, at lower priority. This could be used (for example)
--       by resource loading/unloading queues.
--   - The swapchain extensions is enabled.
--   - No other features are enabled.
makeDeviceCreateInfo :: DeviceAndQueues -> Int -> DeviceCreateInfo '[]
makeDeviceCreateInfo (DeviceAndQueues _h gq pq) auxCount =
  let queueInfo qx qs = SomeStruct $ DeviceQueueCreateInfo () (DeviceQueueCreateFlagBits 0) qx (fromList qs)
      -- the graphics queue CreateInfo needs to include any aux queues at a lower priority
      graphicsQueueInfo = queueInfo gq ([1.0] ++ (replicate auxCount 0.5))
      queueList =
        if (gq == pq)
          then [graphicsQueueInfo]
          else [graphicsQueueInfo, queueInfo pq [1.0]]
      deviceFeatures = zero { samplerAnisotropy = True }
   in DeviceCreateInfo
        ()
        (DeviceCreateFlags 0)
        (fromList (queueList)) -- queues
        (fromList []) -- enabledLayerNames. don't think this is used anymore?
        (fromList [KHR_SWAPCHAIN_EXTENSION_NAME]) -- enabledExtensionNames
        (Just deviceFeatures) -- device features to enable

