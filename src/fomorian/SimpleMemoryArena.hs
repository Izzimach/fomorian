
module Fomorian.SimpleMemoryArena (
  mkSimpleMemoryArena,
  allocBlock,
  returnBlock,
  MemoryBlock,
  SimpleMemoryArena,
  SimpleMemoryArenaConfig(..),
  ArenaStats,
  getArenaStats,
  freeSpace,
  usedSpace
  ) where


import Data.Vector ((!), Vector, fromList, toList)
import qualified Data.Vector as V

data MemoryBlockState = UsedBlock | FreeBlock
  deriving (Eq, Show)

-- | Each chunk of memory is either allocated or free. The type parameter @s@ is the numeric datatype used for size and offset.
data MemoryBlock s =
  MemoryBlock {
    isFree :: MemoryBlockState,
    blockOffset :: s,
    blockSize :: s
  }
  deriving (Eq, Show)

data SimpleMemoryArenaConfig s =
  SimpleMemoryArenaConfig {
    totalSize :: s,
    blockPadding :: s,
    blockAlignment :: s
  }
  deriving (Eq, Show)

data SimpleMemoryArena s =
  SimpleMemoryArena {
    memoryConfig :: SimpleMemoryArenaConfig s,
    blocks :: Vector (MemoryBlock s)
  }
  deriving (Eq, Show)

mkSimpleMemoryArena :: (Integral s) => SimpleMemoryArenaConfig s -> SimpleMemoryArena s
mkSimpleMemoryArena config =
  let initialBlockSize = roundValueDownToAlignment (totalSize config) config
  in
    SimpleMemoryArena {
      memoryConfig = config,
      -- start with a single free memory block
      blocks = (fromList [
                  MemoryBlock {
                    isFree = FreeBlock,
                    blockOffset = 0,
                    blockSize = initialBlockSize
                  }
                ])
    }

roundValueUpToAlignment :: (Integral s) => s -> SimpleMemoryArenaConfig s -> s
roundValueUpToAlignment x config =
  let alignment = (blockAlignment config)
  in roundValueDownToAlignment (x + alignment - 1) config

roundValueDownToAlignment :: (Integral s) => s -> SimpleMemoryArenaConfig s -> s
roundValueDownToAlignment x config =
  let alignment = (blockAlignment config)
  in (x `div` alignment) * alignment

-- | Given a free block, split it into a used block and (possibly) a freed block with the remaining free memory.
splitFreeBlock :: (Integral s) => s ->SimpleMemoryArenaConfig s ->  MemoryBlock s -> Maybe [MemoryBlock s]
-- can't allocate from a used block!
splitFreeBlock _         _      (MemoryBlock UsedBlock _ _)                 = Nothing
splitFreeBlock allocSize config (MemoryBlock FreeBlock origOffset origSize) =
  let alignedAllocSize = roundValueUpToAlignment allocSize config
  in
    if (alignedAllocSize > origSize)
    -- this free block isn't big enough to hold the allocation!
    then Nothing
    else if (alignedAllocSize == origSize)
    -- allocation is the exact size of the free block, so just convert it to a used block
    then Just $ [MemoryBlock UsedBlock origOffset origSize]
    -- alloc a used block at the start of the free block, and put the remaining space in a free block after the used block
    else let b1 = MemoryBlock UsedBlock origOffset alignedAllocSize
             b2 = MemoryBlock FreeBlock (origOffset + alignedAllocSize) (origSize - alignedAllocSize)
         in Just $ [b1,b2]

-- | Apply the given function on successive elements until one succeeds (return a 'Just') and returns the index and result. If no element
--   produces a 'Just' then this returns 'Nothing'
findIndexFirstJust :: (a -> Maybe b) -> Vector a -> Maybe (Int, b)
findIndexFirstJust justTest elems = checkElements justTest elems 0
  where
    checkElements fj v ix =
      if ix >= length v
      then Nothing
      else case fj (v ! ix) of
        Nothing -> checkElements fj v (ix+1)
        Just result -> Just (ix, result)

-- | Find an area of free memory in this arena and allocate it. Returns the updated arena and a 'MemoryBlock' of the newly-allocated region.
--   If it can no free space large enough, return 'Nothing'
allocBlock :: (Integral s) => s -> SimpleMemoryArena s -> Maybe (SimpleMemoryArena s, MemoryBlock s)
allocBlock findSize (SimpleMemoryArena arenaConfig arenaBlocks) =
  -- try to split each block until we find one that succeeds
  case findIndexFirstJust (splitFreeBlock findSize arenaConfig) arenaBlocks of
    Nothing -> Nothing
    Just (ix,splitResult) ->
      let beforeBlocks = V.take ix arenaBlocks
          afterBlocks = V.drop (ix+1) arenaBlocks
          newArena = SimpleMemoryArena arenaConfig (V.concat [beforeBlocks, fromList splitResult, afterBlocks])
          newBlock = head splitResult
      in Just (newArena, newBlock)
                   
mergeBlocks :: (Integral s) => MemoryBlock s -> MemoryBlock s -> MemoryBlock s
mergeBlocks (MemoryBlock u o s) (MemoryBlock _ _ s') = MemoryBlock u o (s+s')


returnBlock :: (Integral s) => MemoryBlock s -> SimpleMemoryArena s -> Maybe (SimpleMemoryArena s)
returnBlock block (SimpleMemoryArena arenaConfig arenaBlocks) =
  case (V.findIndex (== block) arenaBlocks) of
    Nothing -> Nothing
    Just ix -> let beforeBlocks = V.take ix arenaBlocks
                   afterBlocks = V.drop (ix+1) arenaBlocks
                   newBlock = MemoryBlock FreeBlock (blockOffset block) (blockSize block)
               in Just $ SimpleMemoryArena arenaConfig (coalesceBlocks beforeBlocks newBlock afterBlocks)
      where
        coalesceBlocks :: (Integral s) => Vector (MemoryBlock s) -> MemoryBlock s -> Vector (MemoryBlock s) -> Vector (MemoryBlock s)
        coalesceBlocks pre b post =
          -- try to merge with adjacent blocks in post and pre vectors!
          let (b', post') = if (length post > 0 && (isFree (V.head post) == FreeBlock))
                            then (mergeBlocks b (V.head post), V.tail post)
                            else (b, post)
              (b'', pre') = if (length pre > 0 && (isFree (V.last pre) == FreeBlock))
                            then (mergeBlocks b' (V.last pre), V.take ((length pre) - 1) pre)
                            else (b', pre)
          in V.concat [pre',V.singleton b'',post']

data ArenaStats s = 
  ArenaStats {
    freeSpace :: s,
    usedSpace :: s
  }
  deriving (Eq, Show)

-- | Get some useful information about the arena
getArenaStats :: (Integral s) => SimpleMemoryArena s -> ArenaStats s
getArenaStats arena =
  let allBlocks = V.toList (blocks arena)
      isFreeBlock = \b -> isFree b == FreeBlock
      isUsedBlock = \b -> isFree b == UsedBlock
      sumSomeBlocks = \f -> sum $ fmap blockSize (filter f allBlocks)
  in
    ArenaStats (sumSomeBlocks isFreeBlock) (sumSomeBlocks isUsedBlock)