{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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

data WindowBundle = WindowBundle {
    vulkanInstance :: Instance,
    windowHandle :: GLFW.Window,
    vulkanDevice :: Device,
    vulkanSurface :: SurfaceKHR,
    graphicsQueue :: Queue,
    presentQueue :: Queue,
    loaderQueues :: [Queue]
    --swapChainRef :: IORef SwapChainEtc
  }

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
        return ()


  