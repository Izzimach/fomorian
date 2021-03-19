{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Fomorian.Vulkan where

import Data.ByteString (ByteString)
import Data.Foldable
import Data.Word (Word64)
import Data.Bits
import Data.Vector (fromList)

import GHC.Int

import Control.Exception

import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

import Vulkan.Core10
import Vulkan.Zero

import qualified Graphics.UI.GLFW as GLFW

import Fomorian.Windowing

-- | Setup for creating a Vulkan instance. Edit fields here to change your application name or version to use.
--   Notably this config enables surface extensions so that the GLFW function 'createWindowSurface' will work.
instanceConfig :: InstanceCreateInfo '[]
instanceConfig =
  InstanceCreateInfo
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
    (fromList []) -- enabledLayerNames
    (fromList [ "VK_KHR_surface", "VK_KHR_win32_surface" ])  -- extensionNames




main :: IO ()
main = withInstance instanceConfig Nothing bracket $ \i -> do
  --debugInstance i
  let windowConfig = WindowInitData 600 400 "Vulkan test window" NoOpenGL
  w <- initWindow windowConfig
  alloca @Word64 $ \surface -> do
    result <- (GLFW.createWindowSurface (instanceHandle i) w nullPtr surface :: IO GHC.Int.Int32)
    putStrLn $ "Create window result : " ++ (show result)
    if (result >= 0) then
      do
        surfaceHask <- peek @Word64 surface
        putStrLn (show surfaceHask)
    else
      return ()
  
  terminateWindow w




--
-- extra functions to dump vulkan debug text.
--
debugInstance :: Instance -> IO ()
debugInstance i = do
  (_, layerz) <- enumerateInstanceLayerProperties
  (_, extensionz) <- enumerateInstanceExtensionProperties Nothing
  putStrLn (show extensionz)
  putStrLn (show layerz)
  (_, devices) <- enumeratePhysicalDevices i
  traverse_ deviceInfo devices


deviceInfo :: PhysicalDevice -> IO ()
deviceInfo p = do
  (_, extensions) <- enumerateDeviceExtensionProperties p Nothing
  (_, layers    ) <- enumerateDeviceLayerProperties p
  traverse_ myPrint extensions
  --traverse_ myPrint layers
  --myPrint =<< getPhysicalDeviceFeatures p
  --myPrint =<< getPhysicalDeviceProperties p
  --myPrint =<< getPhysicalDeviceMemoryProperties p
  where
    myPrint :: (Show a) => a -> IO ()
    myPrint = putStrLn . show