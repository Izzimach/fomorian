module Fomorian.Vulkan.Resources.DeviceMemoryTypes where

import Data.Bits
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Vector as V
import Data.Word (Word32)

import Vulkan.Core10 as VKCORE

-- | Enum to express preferences for the type of memory to allocate
data AbstractMemoryType =
  -- | Try to find memory that is fast for the GPU to access. This prefers DEVICE_LOCAL memory that isn't HOST_VISIBLE,
  --   but will fall back to other memory types if needed. May not be accessible to the CPU (host). The bulk of
  --   static data (vertex buffers and images) should be from this memory.
    PreferGPU
  -- | Try to find memory that the host (CPU) can access. Prefers HOST_VISIBLE memory that isn't DEVICE_LOCAL, but will fallback to DEVICE_LOCAL.
  --   If CPU code needs to write to buffers/textures this is the type to use.
  --   This memory will probably be slower for the GPU to access so you should use 'PreferGPU' for most things unless you need to constantly write to
  --   the memory region yourself.
  --   Note that the memory is not mapped. You can map it yourself, but this can cause problems since the memory handle is shared and
  --   two threads cannot both map the same memory handle at once. For mapped memory that you write to frequently it's better to use 'AlwaysMapped'
  | RequireHostVisible
  -- | Looks for memory local to the GPU (DEVICE_LOCAL) that's also accessible by the CPU. There is generally not a lot of this kind of memory available
  --   (if any is available!) so it should be used sparingly. If none of this memory is available it falls back on general HOST_VISIBLE memory.
  | GPUAndHostVisible
  -- | Host coherent memory that is always mapped. Used for buffers that are frequenctly written to (like every frame). The mapping/unmapping is done for you,
  --   and the the pointer in 'MemoryAllocation' is set to the 'Just' value pointing at the relevant memory region including the blockOffset.
  | AlwaysMapped
  deriving (Eq, Show, Ord)

-- | Some allocations have special requirements.
data MemoryModifier =
    DefaultMemory      -- ^ just allocate the memory and hand it back
  | MappedArena        -- ^ the whole arena is mapped so all suballocations are always mapped as well
  | UseWholeArena      -- ^ each allocation uses up the whole arena
  deriving (Eq, Ord, Show)

abstractTypeToModifier :: AbstractMemoryType -> MemoryModifier
abstractTypeToModifier AlwaysMapped = MappedArena
abstractTypeToModifier _            = DefaultMemory

newtype MemoryPriorityMap k = MemoryPriorityMap (M.Map k [Word32])
  deriving (Eq, Show)

lookupMemoryPriority :: (Ord k) => MemoryPriorityMap k -> k -> [Word32]
lookupMemoryPriority (MemoryPriorityMap m) k = M.findWithDefault [] k m

-- | A set of functions to describe which memory types are allowed and which ones should be preferred when allocating memory. Make a prioritizer
--   and apply it with 'makePriorityMemoryTypeList' to get a list of allowed memory types sorted with preferred types first.
data MemoryTypePrioritizer = MemoryTypePrioritizer {
  -- | Determines which types are allowed
  isValidMemoryType :: MemoryType -> Bool,
  -- | Provides a sort order for types, with higher ratings preferred over lower ratings
  rateMemoryType :: MemoryType -> Integer
  }

-- | Take a list of memory types and apply a prioritizer to it. Returns indices into the original list of memory types
makePriorityMemoryTypeList :: [MemoryType] -> MemoryTypePrioritizer -> [Word32]
makePriorityMemoryTypeList ts (MemoryTypePrioritizer isValid rateMemory) =
  let tix = zip [(0::Word32)..] ts   -- MemoryType list with indices
      filterIndexed = \(_,t) -> isValid t
      -- We flip the order of t1/t2 when comparing. If we didn't lower values would get sorted to the front and we
      -- want higher values sorted to the front.
      compareIndexed = \(_,t1) (_,t2) -> compare (rateMemory t2) (rateMemory t1)
      sortedIndexed = L.sortBy compareIndexed $ L.filter filterIndexed tix
  in fmap fst sortedIndexed

hasMemoryTypeBit :: MemoryPropertyFlags -> MemoryPropertyFlags -> Bool
hasMemoryTypeBit testee bit1 = (testee .&. bit1) /= zeroBits
      

-- | Prioritizer which prefers fast device local memory
fastPrioritizer :: MemoryTypePrioritizer
fastPrioritizer = MemoryTypePrioritizer {
  -- we will use any memory type
  isValidMemoryType = const True
  , 
  -- We prefer device local memory, and avoid memory  that is device local AND host visible since there generally is not a lot of that available.
  -- Use 'GPUAndHostVisible' if you want host-visible memory on the GPU.
  rateMemoryType = \(MemoryType m _) ->
    let deviceLocal = hasMemoryTypeBit m MEMORY_PROPERTY_DEVICE_LOCAL_BIT
        hostVisible = hasMemoryTypeBit m MEMORY_PROPERTY_HOST_VISIBLE_BIT
    in case (deviceLocal, hostVisible) of
      -- prefer device local that isn't host visible
      (True,False) -> 2
      -- fallback on host visible that isn't on GPU
      (False,True) -> 1
      -- fallback to everything else
      (_,_)        -> 0
}

-- | Prioritizer which only accepts host coherent memory (so we don't have to flush writes), and prefers memory that isn't device local
hostVisibleOnly :: MemoryTypePrioritizer
hostVisibleOnly = MemoryTypePrioritizer {
  -- only allow host visible and coherent memory
  isValidMemoryType = \(MemoryType m _) ->
    (hasMemoryTypeBit m MEMORY_PROPERTY_HOST_VISIBLE_BIT) &&
    (hasMemoryTypeBit m MEMORY_PROPERTY_HOST_COHERENT_BIT)
  ,
  rateMemoryType = \(MemoryType memTypeBits _) ->
    -- prefer host visible memory that isn't on the GPU, since that's generally more scarce
    case hasMemoryTypeBit memTypeBits MEMORY_PROPERTY_DEVICE_LOCAL_BIT of
      False -> 1
      True -> 0                                             
}

-- | Try to use fast (on GPU) host visible memory. If none is available fall back to normal host visible memory
fastHostVisiblePrioritizer :: MemoryTypePrioritizer
fastHostVisiblePrioritizer = MemoryTypePrioritizer {
  -- only allow host visible and coherent memory
  isValidMemoryType = \(MemoryType m _) ->
    (hasMemoryTypeBit m MEMORY_PROPERTY_HOST_VISIBLE_BIT) &&
    (hasMemoryTypeBit m MEMORY_PROPERTY_HOST_COHERENT_BIT)
  ,
  rateMemoryType = \(MemoryType memTypeBits _) ->
    -- prefer host visible memory that IS on the GPU -- opposite of hostVisiblePrioritizer
    case hasMemoryTypeBit memTypeBits MEMORY_PROPERTY_DEVICE_LOCAL_BIT of
      False -> 0
      True -> 1                                            
}



describeMemoryType :: PhysicalDeviceMemoryProperties -> MemoryType -> String
describeMemoryType props (MemoryType m hix) =
  let deviceLocal = hasMemoryTypeBit m MEMORY_PROPERTY_DEVICE_LOCAL_BIT
      hostVisible = hasMemoryTypeBit m MEMORY_PROPERTY_HOST_VISIBLE_BIT
      hostCoherent = hasMemoryTypeBit m MEMORY_PROPERTY_HOST_COHERENT_BIT
      hostCached = hasMemoryTypeBit m MEMORY_PROPERTY_HOST_CACHED_BIT
      showIfBitSet = \b t -> if b then (t ++ " " ) else ""
      (MemoryHeap heapSize _heapFlags) = memoryHeaps props V.! fromIntegral hix
  in
    "MemoryType " ++ "Heap index=" ++ show hix ++ " Heap size=" ++ show heapSize ++ " ["
                  ++ showIfBitSet deviceLocal "DeviceLocal"
                  ++ showIfBitSet hostVisible "HostVisible"
                  ++ showIfBitSet hostCoherent "HostCoherent"
                  ++ showIfBitSet hostCached "HostCached"
                  ++ "]"

describeMemoryOrder :: PhysicalDeviceMemoryProperties -> [Word32] -> String
describeMemoryOrder props order =
  let memTypes = memoryTypes props
      heaps = memoryHeaps props
      orderedTypes = fmap (\ix -> "Memory type " ++ show ix ++ ": " ++ describeMemoryType props (memTypes V.! (fromIntegral ix))) order
  in
    L.intercalate "\n" orderedTypes

-- | Take a VkPhysicalDeviceMemoryProperties and "compile" some data to figure out which memory types to make available and preferred for each memory type.
compileMemoryPriorities :: PhysicalDeviceMemoryProperties -> MemoryPriorityMap AbstractMemoryType
compileMemoryPriorities props =
  let types = V.toList $ memoryTypes props
      memoryTypeChoices = [
                            (PreferGPU, fastPrioritizer),
                            (RequireHostVisible, hostVisibleOnly),
                            -- need a prioritizer for this
                            (GPUAndHostVisible, fastHostVisiblePrioritizer),
                            (AlwaysMapped, hostVisibleOnly)
                          ]
  in MemoryPriorityMap $ M.fromList $ fmap (\(k,v) -> (k, makePriorityMemoryTypeList types v)) memoryTypeChoices
