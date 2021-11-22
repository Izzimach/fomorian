{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Fomorian.Vulkan.Resources.DescriptorSetHelper where

import GHC.Generics

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar

import Data.Proxy
import Data.Kind
import Data.HList.FakePrelude
import Data.HList.HList
import Data.Word (Word32, Word64)
import Data.Vector (Vector, (!?), (//))
import qualified Data.Vector as V
import qualified Data.Map as M

import Foreign.Storable
import Foreign.Ptr

import Linear

import qualified Vulkan.Core10 as VK
import qualified Vulkan.Zero as VZ
import Vulkan.CStruct.Extends


import Fomorian.Vulkan.WindowBundle
import Fomorian.Vulkan.VulkanMonads
import Fomorian.Vulkan.Resources.VulkanResourcesBase
import Fomorian.Vulkan.Resources.DataBuffers

--
-- Handles descriptorsets represented by an HList. Each element of the HList is either:
--  - a 'Storable' data type, in which case the descriptor points to a uniform buffer
--  - an 'ImageSamplerEntry' which is mapped onto the COMBINED_IMAGE_SAMPLER descriptor type

newtype DescriptorHList l = DescriptorHList (HList l)

instance (Show (HList l)) => Show (DescriptorHList l) where
  show (DescriptorHList x) = "DH" ++ show x


data ImageSamplerEntry = ImageSamplerEntry Int Int--VK.ImageView VK.Sampler

data DSType = DSUniform | DSCombinedSampler

type family (EvalDSType a) :: DSType where
  EvalDSType ImageSamplerEntry = 'DSCombinedSampler
  EvalDSType x = 'DSUniform

type family (ToStage (a :: [Type])) :: [Type] where
  ToStage '[] = '[]
  ToStage (x ': xs) = VK.ShaderStageFlagBits ': (ToStage xs)

class HToLayout x s where
  hToLayout :: (SameLength x s, SameLength s x) => Int -> Tagged (HList x) (HList s) -> [DescriptorBinding]

instance HToLayout '[] '[] where
  hToLayout _ (Tagged _) = []

instance (HToLayout l g, SameLength l g, SameLength g l, EvalDSType x ~ (ds :: DSType), HToLayout' ds x) => HToLayout (x ': l) (VK.ShaderStageFlagBits ': g) where
  hToLayout ix (Tagged (HCons h t)) = hToLayout' ix (Proxy :: Proxy ds) (Proxy :: Proxy x) h : hToLayout (ix+1) (Tagged t :: Tagged (HList l) (HList g))


class HToLayout' (ds :: DSType) x where
  hToLayout' :: Int -> Proxy ds -> Proxy x -> VK.ShaderStageFlagBits -> DescriptorBinding

instance HToLayout' 'DSCombinedSampler x where
  hToLayout' ix _ _ stage = CombinedDescriptor (fromIntegral ix) stage 1 V.empty

instance (Storable x) => HToLayout' 'DSUniform x where
  hToLayout' ix _ _ stage = UniformDescriptor (fromIntegral ix) stage (fromIntegral $ sizeOf @x undefined) (fromIntegral $ alignment @x undefined)

dSetTypeToInfo :: forall d s. (SameLength d s, SameLength s d, HToLayout d s) => Proxy (HList d) -> HList s -> DescriptorSetInfo
dSetTypeToInfo _ stages = DescriptorSetInfo $ hToLayout 0 (Tagged @(HList d) stages)


-- process for handling descriptor sets with the helper:
--
-- For the default shader used we have two descriptors:
--  - a uniform buffer with transforms. We'll build a record that support 'Storable'
--  - a combined image/sampler descriptor. This is represented by a ImageSamplerEntry
--

data HelperExample = HelperExample
  {
    uboModel :: M44 Float,
    uboView  :: M44 Float,
    uboProjection :: M44 Float
  }
  deriving (Eq,Show,Generic)

instance Storable HelperExample where
  sizeOf _ = sizeOf @(M44 Float) undefined + sizeOf @(M44 Float) undefined + sizeOf @(M44 Float) undefined
  alignment _ = 16 -- maybe only needs to be 4?
  peek p = do
    -- treat as an array of @M44 Float@ objects
    let pMats = castPtr @HelperExample @(M44 Float) p
    m <- peekElemOff pMats 0
    v <- peekElemOff pMats 1
    p <- peekElemOff pMats 2
    return (HelperExample m v p)
  poke p (HelperExample m v prj) = do
    let pMats = castPtr @HelperExample @(M44 Float) p
    pokeElemOff pMats 0 m
    pokeElemOff pMats 1 v
    pokeElemOff pMats 2 prj

type HelperDescriptorFields = '[HelperExample, ImageSamplerEntry]

helperDescriptorInfo :: DescriptorSetInfo
helperDescriptorInfo = dSetTypeToInfo (Proxy @(HList HelperDescriptorFields)) (hEnd $ hBuild VK.SHADER_STAGE_VERTEX_BIT VK.SHADER_STAGE_FRAGMENT_BIT)


makeDescriptorSetHelperSource ::  (InVulkanMonad effs) => DescriptorSetInfo -> VK.DescriptorSetLayout -> Eff effs DescriptorSetHelperSource
makeDescriptorSetHelperSource dsi dsl = do
  metaPoolVar <- sendM $ newTMVarIO M.empty
  return (DescriptorSetHelperSource metaPoolVar dsi dsl)


useDescriptorSetHelperSource :: (InVulkanMonad effs) => DescriptorSetHelperSource -> Int -> Eff (State DescriptorSetHelper ': effs) x -> Eff effs x
useDescriptorSetHelperSource (DescriptorSetHelperSource poolVar info layout) poolKey poolOp = do
  allHelpers <- sendM $ atomically $ takeTMVar poolVar
  -- find the relevant pool, or create it
  helper <- case M.lookup poolKey allHelpers of
            Just p -> return p
            Nothing -> makeDescriptorSetHelper info layout 10
  (x, helper') <- runState helper poolOp
  sendM $ atomically $ putTMVar poolVar (M.insert poolKey helper' allHelpers)
  return x

destroyDescriptorSetHelperSource ::  (InVulkanMonad effs) => DescriptorSetHelperSource -> Eff effs ()
destroyDescriptorSetHelperSource (DescriptorSetHelperSource poolVar info layout) = do
  allHelpers <- sendM $ atomically $ takeTMVar poolVar
  mapM_ destroyDescriptorSetHelper allHelpers

makeDescriptorSetHelper :: (InVulkanMonad effs) => DescriptorSetInfo -> VK.DescriptorSetLayout -> Int -> Eff effs DescriptorSetHelper
makeDescriptorSetHelper info layout chunkSize = return $ DescriptorSetHelper V.empty V.empty info layout chunkSize

destroyDescriptorSetHelper :: (InVulkanMonad effs) => DescriptorSetHelper -> Eff effs ()
destroyDescriptorSetHelper = undefined

nextDescriptorSetBundle :: (InVulkanMonad effs, Member (State DescriptorSetHelper) effs) => Eff effs DescriptorHelperBundle
nextDescriptorSetBundle = do
  h <- get @DescriptorSetHelper
  case (freeSubHelpers h) !? 0 of
    Nothing -> makeNewPool h
    Just sh -> do
      case getFromPool sh of
        Nothing -> do
          put $ h {
            freeSubHelpers = V.tail (freeSubHelpers h),
            usedSubHelpers = V.cons sh (usedSubHelpers h)
          }
          -- run again, maybe the next freeSubHelper will work
          nextDescriptorSetBundle
        Just (dSet, sh') -> do
          let h' = h { freeSubHelpers = (freeSubHelpers h) // [(0,sh')]}
          put h'
          return dSet
  where
    makeNewPool h = do
      newPool <- makeDescriptorSetHelperPool h
      case getFromPool newPool of
        Nothing -> error "argh" -- WTF
        Just (dSet,pool') -> do
          put $ h { freeSubHelpers = V.cons pool' (freeSubHelpers h) }
          return dSet

resetDescriptorSetHelper :: (InVulkanMonad effs, Member (State DescriptorSetHelper) effs) => Eff effs ()
resetDescriptorSetHelper = do
  h <- get @DescriptorSetHelper
  let allSubHelpers = (freeSubHelpers h) V.++ (usedSubHelpers h)
  resetPools <- mapM resetHelperPool allSubHelpers
  put $ h { freeSubHelpers = resetPools, usedSubHelpers = V.empty }
  return ()


makeDescriptorSetHelperPool :: (InVulkanMonad effs) => DescriptorSetHelper -> Eff effs DescriptorSetHelperPool
makeDescriptorSetHelperPool dHelp@(DescriptorSetHelper _ _ info layout setCount) = do
  d <- getDevice
  -- the physical device limits alignment of uniform buffers, so we need to take that into account when packing uniform buffers into
  -- our memory blocks
  uniformBufferAlignment <- VK.minUniformBufferOffsetAlignment . VK.limits . physicalDeviceProperties . vulkanDeviceBundle <$> getWindowBundle
  let poolSizes = findPoolSizesFromInfo info
      bufStrides = findUniformStrides info uniformBufferAlignment
      dCreateInfo = VK.DescriptorPoolCreateInfo () VZ.zero (fromIntegral setCount) (V.fromList $ M.elems poolSizes)

  -- create the descriptor pool and allocate all the descriptor sets (should use up the whole pool)
  dPool <- VK.createDescriptorPool d dCreateInfo Nothing
  let allocateInfo = VK.DescriptorSetAllocateInfo () dPool (V.fromList (replicate setCount layout))
  dSets <- VK.allocateDescriptorSets d allocateInfo

  -- allocate uniform buffers for those descriptors that reference one (a vector of Maybe UBuffer)
  uBuffers <- mapM makeUniformBlock bufStrides

  -- now build the DescriptorHelperBundles. Each bundle should have a descriptor set and
  -- some uniform buffers. We should also set up the descriptors to point to their uniform buffer data
  bundles <- mapM (createBundle dSets uBuffers info bufStrides) [0..(setCount-1)]
  mapM_ (bindBundleUniforms info) bundles

  return (DescriptorSetHelperPool (V.fromList bundles) V.empty dPool (V.fromList uBuffers))
    where
      -- convert all the descriptor bindings into a map of poolsize requests and accumulate into a map
      findPoolSizesFromInfo (DescriptorSetInfo bindings) = foldr addBindingToPoolSizes M.empty bindings

      -- for each binding we need to request some amount of descriptors in the pool
      addBindingToPoolSizes b sizeMap =
        let (dType, requestSize :: Word32) =
          case b of
            (UniformDescriptor bindIndex stages size align) -> (VK.DESCRIPTOR_TYPE_UNIFORM_BUFFER, fromIntegral $ setCount)
            (CombinedDescriptor bindIndex stages count im)  -> (VK.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, (count * fromIntegral setCount))
        in case M.lookup dType sizeMap of
          Nothing                                      -> M.insert dType (VK.DescriptorPoolSize dType requestSize) sizeMap
          Just (VK.DescriptorPoolSize dType prevCount) -> M.insert dType (VK.DescriptorPoolSize dType (prevCount + requestSize)) sizeMap

      findUniformStrides (DescriptorSetInfo bindings) minAlign = fmap (bindingToMemStride minAlign) bindings

      bindingToMemStride minAlignment (UniformDescriptor _ _ size align) = Just $ alignedSize size (max align minAlignment)
      bindingToMemStride _ (CombinedDescriptor _ _ _ _)                  = Nothing

      alignedSize :: VK.DeviceSize -> VK.DeviceSize -> VK.DeviceSize
      alignedSize size align = ((size + align - 1) `div` align) * align

      makeUniformBlock Nothing = return Nothing
      makeUniformBlock (Just stride) = Just <$> makeUniformBuffer (stride * fromIntegral setCount)

      createBundle dSets uBuffers info bufStrides ix = do
        let dSet = dSets V.! ix
            bufData = fmap (calcOffset ix) $ zip uBuffers bufStrides
        return (DescriptorHelperBundle dSet (V.fromList bufData))

      calcOffset :: Int -> (Maybe UBuffer, Maybe VK.DeviceSize) -> Maybe (UBuffer, VK.DeviceSize)
      calcOffset ix (Just uBuf, Just str) = Just (uBuf, str * fromIntegral ix)
      calcOffset _  _                     = Nothing

      bindBundleUniforms (DescriptorSetInfo bindings) (DescriptorHelperBundle dSet bufData) = do
        d <- getDevice
        mapM_ (calcDescriptorWrite dSet) $ zip bindings (V.toList bufData)

      calcDescriptorWrite dSet ((UniformDescriptor bindIndex _ size _), Just ((UBuffer uBuf _),offset)) = do
        d <- getDevice
        let bufferInfo = VK.DescriptorBufferInfo uBuf offset (fromIntegral size)
            writeStruct = SomeStruct $ VK.WriteDescriptorSet () dSet bindIndex 0 1 VK.DESCRIPTOR_TYPE_UNIFORM_BUFFER V.empty (V.singleton bufferInfo) V.empty
        VK.updateDescriptorSets d (V.singleton writeStruct) V.empty

      calcDescriptorWrite _ (_,_)                                                                      = return ()

getFromPool :: DescriptorSetHelperPool -> Maybe (DescriptorHelperBundle, DescriptorSetHelperPool)
getFromPool = undefined

resetHelperPool :: (InVulkanMonad effs) => DescriptorSetHelperPool -> Eff effs DescriptorSetHelperPool
resetHelperPool = undefined