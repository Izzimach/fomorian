{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Fomorian.Vulkan.Vulkan where

import Data.ByteString (ByteString, readFile, pack)
import Data.Foldable
import Data.IORef
import Data.Word (Word32, Word64)
import Data.Bits
import Data.Vector (Vector(..), fromList, findIndex, (!), empty, (//))

import GHC.Int

import System.FilePath

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

import Vulkan.Core10
import Vulkan.Core10.DeviceInitialization
import Vulkan.Zero
import Vulkan.CStruct.Extends
import Vulkan.Exception
import Vulkan.Extensions.VK_KHR_surface as VKSURFACE
import Vulkan.Extensions.VK_KHR_win32_surface
import Vulkan.Extensions.VK_KHR_swapchain as VKSWAPCHAIN
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_EXT_validation_features

import qualified Graphics.UI.GLFW as GLFW

import Fomorian.Windowing

-- | This is a C function used as a callback to handle the vulkan validation messages.
foreign import ccall unsafe "SomeCode.c &debugCallback" debugCallbackPtr :: PFN_vkDebugUtilsMessengerCallbackEXT

data VulkanValidation = UseValidation | NoValidation

cMAX_FRAMES_IN_FLIGHT :: Int
cMAX_FRAMES_IN_FLIGHT = 2

-- | Setup for creating a Vulkan instance. Edit fields here to change your application name or version to use.
--   Notably this config enables surface extensions so that the GLFW function 'createWindowSurface' will work.
instanceConfig :: InstanceCreateInfo '[] -- '[DebugUtilsMessengerCreateInfoEXT, ValidationFeaturesEXT]
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
        empty -- enabledLayerNames
        (fromList [ KHR_SURFACE_EXTENSION_NAME, KHR_WIN32_SURFACE_EXTENSION_NAME ])  -- extensionNames

-- | Modifies the standard InstanceCreateInfo to add validation layers.
validatedInstance :: InstanceCreateInfo '[] -> InstanceCreateInfo '[DebugUtilsMessengerCreateInfoEXT, ValidationFeaturesEXT]
validatedInstance baseCreateInfo = 
  let debugCreateInfo = DebugUtilsMessengerCreateInfoEXT zero
        (DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT)
        (DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT)
        debugCallbackPtr
        nullPtr
      validationFeatures = ValidationFeaturesEXT (fromList [VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT]) empty
      debugLayers = fromList [ "VK_LAYER_KHRONOS_validation" ]
      debugExtensions =  (Vulkan.Core10.DeviceInitialization.enabledExtensionNames baseCreateInfo) <>
                         (fromList [ EXT_VALIDATION_FEATURES_EXTENSION_NAME, EXT_DEBUG_UTILS_EXTENSION_NAME ])
  in
    (baseCreateInfo {
      Vulkan.Core10.DeviceInitialization.enabledLayerNames = debugLayers,
      Vulkan.Core10.DeviceInitialization.enabledExtensionNames = debugExtensions
    })
    ::& (debugCreateInfo :& validationFeatures :& ())




main :: IO ()
main = do
    --let config = instanceConfig
    let config = validatedInstance instanceConfig
    let allocator = Nothing
    withInstance config allocator bracket $ \i -> do
      --debugInstance i
      let windowConfig = WindowInitData 600 400 "Vulkan test window" NoOpenGL
      withWindowEtc i windowConfig bracket $ \wETC@(WindowEtc inst handle dev phy surf gq pq is rs fnc cpool swref) -> do
        renderLoop wETC allocator
        deviceWaitIdle dev


renderLoop :: WindowEtc -> Maybe AllocationCallbacks -> IO ()
renderLoop windowEtc allocator = do 
    inFlightInit >>= go 0
    return ()
  where
    inFlightInit :: IO (Vector Fence)
    inFlightInit = do
      swEtc <- readIORef (swapChainRef windowEtc)
      let imageCount = length (swapchainFramebuffers swEtc)
      return $ fromList $ fmap (\_ -> NULL_HANDLE) [1..imageCount]
    go currentFrame inFlight = do
      GLFW.pollEvents
      x <- GLFW.windowShouldClose (windowHandle windowEtc)
      if x then
        return ()
      else do
        inFlight' <- renderFrame windowEtc currentFrame inFlight allocator
        go ((currentFrame + 1) `mod` cMAX_FRAMES_IN_FLIGHT) inFlight'

renderFrame :: WindowEtc -> Int -> Vector Fence -> Maybe AllocationCallbacks -> IO (Vector Fence)
renderFrame windowEtc currentFrame inFlight allocator = do
  swapchainEtc <- readIORef (swapChainRef windowEtc)
  let device = vkDevice windowEtc
  let swap = theSwapchain swapchainEtc
  let iaSemaphore = imageAvailableSemaphores windowEtc ! currentFrame
  let sgSemaphore = renderingFinishedSemaphores windowEtc ! currentFrame
  let thisFence = fences windowEtc ! currentFrame
  waitForFences device (fromList [thisFence]) True maxBound
  runResult <- try $
    do
      (_, imageIndex) <- acquireNextImageKHR device swap maxBound iaSemaphore NULL_HANDLE 
      let imageFence = inFlight ! fromIntegral imageIndex
      if (imageFence /= NULL_HANDLE)
      then
        waitForFences device (fromList [imageFence]) True maxBound
      else
        return SUCCESS
      let buffer = (swapchainCommandBuffers swapchainEtc) ! (fromIntegral imageIndex)
      let submitInfo = SomeStruct $ SubmitInfo () (fromList [iaSemaphore]) 
                                                  (fromList [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT])
                                                  (fromList [commandBufferHandle buffer])
                                                  (fromList [sgSemaphore])
      resetFences device (fromList [thisFence])
      queueSubmit (graphicsQueue windowEtc) (fromList [submitInfo]) thisFence
      let presentInfo = PresentInfoKHR () (fromList [sgSemaphore])
                                          (fromList [theSwapchain swapchainEtc])
                                          (fromList [imageIndex])
                                          nullPtr
      queuePresentKHR (presentQueue windowEtc) presentInfo
      return $ inFlight // [(fromIntegral imageIndex, thisFence)]
  case runResult of
    Right result -> return result
    Left (VulkanException ERROR_OUT_OF_DATE_KHR) -> do
        let chosen = vkChosen windowEtc
        let cpool = (cmdPool windowEtc)
        deviceWaitIdle device
        newswapchain <- recreateSwapChainEtc device cpool swapchainEtc chosen (surfaceRef windowEtc) allocator
        liftIO $ writeIORef (swapChainRef windowEtc) newswapchain
        return inFlight
    Left exc -> throw exc


data WindowEtc =
  WindowEtc {
    vkInstance :: Instance,
    windowHandle :: GLFW.Window,
    vkDevice :: Device,
    vkChosen :: ChosenDevice,
    surfaceRef :: SurfaceKHR,
    graphicsQueue :: Queue,
    presentQueue :: Queue,
    imageAvailableSemaphores :: Vector Semaphore,
    renderingFinishedSemaphores :: Vector Semaphore,
    fences :: Vector Fence,
    cmdPool :: CommandPool,
    swapChainRef :: IORef SwapchainEtc
  }

-- | Holds a swapchain and the associated data handles. 
data SwapchainEtc =
  SwapchainEtc {
    theSwapchain :: SwapchainKHR,
    swapchainImages :: Vector Image,
    swapchainImageViews :: Vector ImageView,
    swapchainFramebuffers :: Vector Framebuffer,
    swapchainCommandBuffers :: Vector CommandBuffer,
    swapchainPipeline :: PipelineEtc,
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
    simpleSemaphoreConfig = (SemaphoreCreateInfo () zero)
    simpleFenceConfig = (FenceCreateInfo () FENCE_CREATE_SIGNALED_BIT)

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
            withSyncObjects device simpleSemaphoreConfig simpleFenceConfig Nothing cMAX_FRAMES_IN_FLIGHT wrapper $ \syncs -> do
              let (SyncObjects imageAvailables renderingFinisheds fences) = syncs
              let commandPoolInfo = CommandPoolCreateInfo zero gq
              withCommandPool device commandPoolInfo Nothing wrapper $ \cmdpool -> do
                swapchainInfo <- generateSwapchainInfo c surfaceH
                withSwapchainEtc device swapchainInfo cmdpool Nothing wrapper $ \swapchainEtc -> do
                    wrapped (WindowEtc inst w device c surfaceH graphicsQ presentQ imageAvailables renderingFinisheds fences cmdpool swapchainEtc)


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
withSwapchainEtc :: (MonadIO io) => Device -> SwapchainCreateInfoKHR '[] -> CommandPool -> Maybe AllocationCallbacks -> (io (IORef SwapchainEtc) -> (IORef SwapchainEtc -> io ()) -> r) -> r
withSwapchainEtc device createInfo cpool allocator wrapper = wrapper startSwapchainRef endSwapchainRef
  where
    startSwapchainRef :: (MonadIO io) => io (IORef SwapchainEtc)
    startSwapchainRef =
      do
        swETC <- createSwapchainEtc device cpool createInfo allocator
        liftIO $ newIORef swETC
    endSwapchainRef :: (MonadIO io) => IORef SwapchainEtc -> io ()
    endSwapchainRef ref =
      do
        swETC <- liftIO $ readIORef ref
        destroySwapchainEtc device cpool allocator swETC

createSwapchainEtc :: (MonadIO io) => Device -> CommandPool -> SwapchainCreateInfoKHR '[] -> Maybe AllocationCallbacks -> io SwapchainEtc
createSwapchainEtc device cpool createInfo allocator = do
  swapchain <- createSwapchainKHR device createInfo allocator
  (_,newImages) <- getSwapchainImagesKHR device swapchain
  newImageViews <- createImageViews device createInfo newImages
  pipe <- liftIO $ buildSimplePipeline device createInfo
  framebuffers <- makeFramebuffers device pipe createInfo newImageViews
  cmdBuffers <- makeCommandBuffers device cpool framebuffers
  liftIO $ recordCommandBuffers cmdBuffers framebuffers createInfo pipe
  return (SwapchainEtc swapchain newImages newImageViews framebuffers cmdBuffers pipe createInfo)

destroySwapchainEtc :: (MonadIO io) => Device -> CommandPool -> Maybe AllocationCallbacks -> SwapchainEtc -> io ()
destroySwapchainEtc device cpool allocator swETC = do
  let (SwapchainEtc swapchain images imageViews framebuffers commandbuffers pipe _) = swETC
  mapM (\fb -> destroyFramebuffer device fb Nothing) framebuffers
  freeCommandBuffers device cpool commandbuffers
  liftIO $ destroyPipelineEtc device pipe
  mapM (\iv -> destroyImageView device iv Nothing) imageViews
  destroySwapchainKHR device swapchain allocator

recreateSwapChainEtc :: (MonadIO io) => Device -> CommandPool -> SwapchainEtc -> ChosenDevice -> SurfaceKHR -> Maybe AllocationCallbacks -> io SwapchainEtc
recreateSwapChainEtc device cpool oldswETC chosen surfaceK allocator = do
  destroySwapchainEtc device cpool allocator oldswETC
  newCreateInfo <- liftIO $ generateSwapchainInfo chosen surfaceK
  createSwapchainEtc device cpool newCreateInfo allocator

createImageViews :: (MonadIO io) => Device -> SwapchainCreateInfoKHR '[] -> Vector Image -> io (Vector ImageView)
createImageViews device swapchainInfo images =
  let idSw = COMPONENT_SWIZZLE_IDENTITY
      mkImageCreateInfo i = ImageViewCreateInfo 
                              () 
                              zero 
                              i 
                              IMAGE_VIEW_TYPE_2D
                              (imageFormat swapchainInfo) 
                              (ComponentMapping idSw idSw idSw idSw)
                              (ImageSubresourceRange IMAGE_ASPECT_COLOR_BIT 0 1 0 1)
      mkView img = createImageView device (mkImageCreateInfo img) Nothing
  in
    traverse mkView images

makeFramebuffers :: (MonadIO io) => Device -> PipelineEtc -> SwapchainCreateInfoKHR '[] -> Vector ImageView -> io (Vector Framebuffer) 
makeFramebuffers device pipe sce imageViews = do
  let (Extent2D w h) = (VKSWAPCHAIN.imageExtent $ sce)
  let rPass = rendPass pipe 
  let mkFramebuffers iv = createFramebuffer device (FramebufferCreateInfo () zero rPass (fromList [iv]) w h 1) Nothing
  fbs <- mapM mkFramebuffers imageViews
  return fbs

makeCommandBuffers :: (MonadIO io) => Device -> CommandPool -> Vector Framebuffer  -> io (Vector CommandBuffer)
makeCommandBuffers device cmdpool fb = do
  let count = fromIntegral $ length fb
  let allocInfo =  CommandBufferAllocateInfo cmdpool COMMAND_BUFFER_LEVEL_PRIMARY count
  buffers <- allocateCommandBuffers device allocInfo
  return buffers

recreateSwapchainEtc :: Device -> IORef SwapchainEtc -> IO ()
recreateSwapchainEtc device swref = do
  swapchainStuff <- readIORef swref
  (SUCCESS,newImages) <- getSwapchainImagesKHR device (theSwapchain swapchainStuff)
  let newswapchainStuff = swapchainStuff { swapchainImages = newImages };
  writeIORef swref newswapchainStuff
  return ()

data SyncObjects = SyncObjects (Vector Semaphore) (Vector Semaphore) (Vector Fence)
  deriving Show

withSyncObjects :: (MonadIO io) => 
  Device -> SemaphoreCreateInfo '[] -> FenceCreateInfo '[] -> Maybe AllocationCallbacks -> Int ->
  (io SyncObjects -> (SyncObjects -> io ()) -> r) -> r
withSyncObjects device semcreate fencecreate alloc count wrapper = wrapper startSyncObjects endSyncObjects
  where
    startSyncObjects :: (MonadIO io) => io SyncObjects
    startSyncObjects = do
      let counts = [1..count]
      isems <- mapM (\_ -> createSemaphore device semcreate alloc) counts
      rsems <- mapM (\_ -> createSemaphore device semcreate alloc) counts
      fences <- mapM (\_ -> createFence device fencecreate alloc) counts
      return $ SyncObjects (fromList isems) (fromList rsems) (fromList fences)
    endSyncObjects ::  (MonadIO io) => SyncObjects -> io ()
    endSyncObjects (SyncObjects isems rsems fences) = do
      mapM_ (\s -> destroySemaphore device s alloc) isems
      mapM_ (\s -> destroySemaphore device s alloc) rsems
      mapM_ (\f -> destroyFence device f alloc) fences




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



data PipelineEtc = PipelineEtc  {
    pipeline :: Pipeline,
    rendPass :: RenderPass,
    pipeLayout :: PipelineLayout,
    shaderModules :: Vector ShaderModule
  }
  deriving Show


-- | Read in SPIR-V shader files
readSPIRV :: Device -> FilePath -> IO (ShaderModule, ShaderModule)
readSPIRV dev shaderPath = do
  let vertPath = "resources" </> "shaders" </> (shaderPath ++ "vert.spv")
  vertBytes <- Data.ByteString.readFile vertPath
  vertModule <- createShaderModule dev (ShaderModuleCreateInfo () zero vertBytes) Nothing
  let fragPath = "resources" </> "shaders" </> (shaderPath ++ "frag.spv")
  fragBytes <- Data.ByteString.readFile fragPath
  fragModule <- createShaderModule dev (ShaderModuleCreateInfo () zero fragBytes) Nothing
  return (vertModule, fragModule)

-- | make a createinfo for a given shader module
shaderStageBoilerplate :: ShaderModule -> ByteString -> SomeStruct (PipelineShaderStageCreateInfo)
shaderStageBoilerplate sm entry = SomeStruct $
  PipelineShaderStageCreateInfo () zero SHADER_STAGE_VERTEX_BIT sm entry Nothing

buildSimplePipeline :: Device -> SwapchainCreateInfoKHR '[] -> IO PipelineEtc
buildSimplePipeline device swapchainInfo = do
  (vm,fm) <- readSPIRV device "tut"
  let shaderStages = fromList [
                      SomeStruct $ PipelineShaderStageCreateInfo () zero SHADER_STAGE_VERTEX_BIT vm "main" Nothing,
                      SomeStruct $ PipelineShaderStageCreateInfo () zero SHADER_STAGE_FRAGMENT_BIT fm "main" Nothing
                      ]
  let vertexStageInfo = PipelineVertexInputStateCreateInfo () zero empty empty
  let inputAssembly = PipelineInputAssemblyStateCreateInfo zero PRIMITIVE_TOPOLOGY_TRIANGLE_LIST False
  let windowExtent@(Extent2D w h) = VKSWAPCHAIN.imageExtent swapchainInfo
  let viewport = Viewport 0.0 0.0 (fromIntegral w) (fromIntegral h) 0.0 1.0
  let scissor = Rect2D (Offset2D 0 0) windowExtent
  let viewportState = PipelineViewportStateCreateInfo () zero 1 (fromList [viewport]) 1 (fromList [scissor])
  -- use simple fill mode and culling
  let rasterizerState = PipelineRasterizationStateCreateInfo () zero False False POLYGON_MODE_FILL CULL_MODE_BACK_BIT FRONT_FACE_CLOCKWISE False 0 0 0 1.0
  -- multisample is basically disabled
  let multisampleState = PipelineMultisampleStateCreateInfo () zero SAMPLE_COUNT_1_BIT False 1.0 empty False False
  let blendAllBits = (COLOR_COMPONENT_R_BIT .|. COLOR_COMPONENT_G_BIT .|. COLOR_COMPONENT_B_BIT .|. COLOR_COMPONENT_A_BIT)
  let colorBlendState = PipelineColorBlendAttachmentState False BLEND_FACTOR_ONE BLEND_FACTOR_ZERO BLEND_OP_ADD 
                                                                BLEND_FACTOR_ONE BLEND_FACTOR_ZERO BLEND_OP_ADD 
                                                                blendAllBits
  let colorBlendCreate = PipelineColorBlendStateCreateInfo () zero False LOGIC_OP_COPY (fromList [colorBlendState]) (0,0,0,0)
  let dynamicStateCreate = PipelineDynamicStateCreateInfo zero (fromList [DYNAMIC_STATE_VIEWPORT, DYNAMIC_STATE_LINE_WIDTH])
  -- layout is empty for now
  let pipelineLayoutCreate = PipelineLayoutCreateInfo zero empty empty
  pipelineLayout <- createPipelineLayout device pipelineLayoutCreate Nothing

  let swapchainFormat = VKSWAPCHAIN.imageFormat swapchainInfo
  let attachmentDescription = AttachmentDescription zero swapchainFormat
                                SAMPLE_COUNT_1_BIT
                                ATTACHMENT_LOAD_OP_CLEAR
                                ATTACHMENT_STORE_OP_STORE
                                ATTACHMENT_LOAD_OP_DONT_CARE
                                ATTACHMENT_STORE_OP_DONT_CARE
                                IMAGE_LAYOUT_UNDEFINED
                                IMAGE_LAYOUT_PRESENT_SRC_KHR
  let attachmentReference = AttachmentReference 0 IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
  let subpassDescription = SubpassDescription zero PIPELINE_BIND_POINT_GRAPHICS
                             empty                               -- inputAttachments
                             (fromList [attachmentReference])    -- colorAttachments
                             empty                               -- resolveAttachments
                             Nothing                             -- depthStencilAttachment
                             empty                               -- preserveAttachments
  let subpassDependency = SubpassDependency SUBPASS_EXTERNAL
                                            0
                                            PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                                            PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                                            zero
                                            ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                                            zero
  let renderPassCreateInfo = RenderPassCreateInfo 
                               ()
                               zero
                               (fromList [attachmentDescription])
                               (fromList [subpassDescription])
                               (fromList [subpassDependency])
  renderPass <- createRenderPass device renderPassCreateInfo Nothing

  let pipelineCreateInfo = GraphicsPipelineCreateInfo
                             ()                                    -- next
                             zero                                  -- flags
                             shaderStages                          -- stages
                             (Just (SomeStruct $ vertexStageInfo)) -- vertexInputState
                             (Just inputAssembly)                  -- inputAssemblyState
                             Nothing                               -- tessellationState
                             (Just (SomeStruct $ viewportState))   -- viewportState
                             (SomeStruct rasterizerState)          -- rasterizationState
                             (Just (SomeStruct multisampleState))  -- multisampleState
                             Nothing                               -- depthStencilState
                             (Just (SomeStruct colorBlendCreate))   -- colorBlendState
                             Nothing                               -- dynamicState
                             pipelineLayout                        -- layout
                             renderPass                            -- renderPass
                             0                                     -- subpass
                             NULL_HANDLE                           -- basePipelineHandle
                             (-1)                                  -- basePipelineIndex
  (_,pipelines) <- createGraphicsPipelines device NULL_HANDLE (fromList [SomeStruct pipelineCreateInfo]) Nothing
  let onepipe = pipelines ! 0
  return $ PipelineEtc onepipe renderPass pipelineLayout (fromList [vm,fm]) 

destroyPipelineEtc :: Device -> PipelineEtc -> IO ()
destroyPipelineEtc dev (PipelineEtc pipeline renderPass layout modules) = do
  mapM_ (\m -> destroyShaderModule dev m Nothing) modules
  destroyPipelineLayout dev layout Nothing
  destroyRenderPass dev renderPass Nothing
  destroyPipeline dev pipeline Nothing

recordCommandBuffers :: Vector CommandBuffer -> Vector Framebuffer -> SwapchainCreateInfoKHR '[] -> PipelineEtc -> IO ()
recordCommandBuffers cbs fbs sci pipeETC = do
  let buffers = zip (toList cbs) (toList fbs)
  let goBuffers (cb,fb) = recordCommands cb sci (rendPass pipeETC) (pipeline pipeETC) fb
  mapM_ goBuffers buffers
  return ()

recordCommands :: CommandBuffer -> SwapchainCreateInfoKHR '[] -> RenderPass -> Pipeline -> Framebuffer -> IO ()
recordCommands buf sce pass pipe fb = do
  beginCommandBuffer buf (CommandBufferBeginInfo () zero Nothing)
  let renderarea = Rect2D (Offset2D 0 0) (VKSWAPCHAIN.imageExtent $ sce)
  let clearTo = fromList [Color (Float32 0 0 0 1)]
  cmdBeginRenderPass buf (RenderPassBeginInfo () pass fb renderarea clearTo) SUBPASS_CONTENTS_INLINE
  cmdBindPipeline buf PIPELINE_BIND_POINT_GRAPHICS pipe
  cmdDraw buf 3 1 0 0
  cmdEndRenderPass buf
  endCommandBuffer buf

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
