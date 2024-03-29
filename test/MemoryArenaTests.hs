{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module MemoryArenaTests where

import qualified Data.Map.Strict as M
import Data.Vector (length)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Fomorian.SimpleMemoryArena

memoryArenaTests :: IO Bool
memoryArenaTests = checkSequential $$(discover)

basicArenaConfig :: SimpleMemoryArenaConfig Int
basicArenaConfig = SimpleMemoryArenaConfig 100000 4

chainAllocations :: Integral s => [s] -> MemoryAlignment s -> SimpleMemoryArena s -> Maybe (SimpleMemoryArena s, [MemoryBlock s])
chainAllocations sz alignment ar = accumulateAllocations sz alignment ar []
  where
    accumulateAllocations []     align arena x = Just (arena, x)
    accumulateAllocations (a:as) align arena x =
      case allocBlock a align arena of
        Nothing -> Nothing
        Just (arena', newBlock) -> accumulateAllocations as align arena' (newBlock : x)

chainReturns :: Integral s => [MemoryBlock s] -> SimpleMemoryArena s -> Maybe (SimpleMemoryArena s)
chainReturns blks ar = accumulateReturns blks ar
  where
    accumulateReturns [] arena = Just arena
    accumulateReturns (b:bs) arena =
      case returnBlock b arena of
        Nothing -> Nothing
        Just arena' -> accumulateReturns bs arena'

-- | A single allocation of any size not exceeding the arena size should succeed
prop_singleAlloc :: Property
prop_singleAlloc = property $ do
  let basicArena = mkSimpleMemoryArena basicArenaConfig
  let alignment = MemAlign 4
  allocSize <- forAll $ Gen.int (Range.linear 1 10000)
  case allocBlock allocSize alignment basicArena of
    Nothing -> do footnote $ "Error attempting to make a single allocation"
                  failure
    Just _  -> pure ()

-- | A single allocation followed by freeing that allocation should result in 0 used space
prop_singleAllocReturn :: Property
prop_singleAllocReturn = property $ do
  let basicArena = mkSimpleMemoryArena basicArenaConfig
  let alignment = MemAlign 4
  allocSize <- forAll $ Gen.int (Range.linear 1 10000)
  case allocBlock allocSize alignment basicArena of
    Nothing -> do footnote $ "Error attempting to make a single allocation"
                  failure
    Just (arena', block) ->
      case returnBlock block arena' of
        Nothing -> failure
        Just arena'' -> assert (usedSpace (getArenaStats arena'') == 0)

-- | We do several allocations that should be within the available capacity of the arena, so all allocations should succeed
prop_withinCapacity :: Property
prop_withinCapacity = property $ do
  let basicArena = mkSimpleMemoryArena basicArenaConfig
  let alignment = MemAlign 4
  -- make alloc request used up to a quarter of the free space
  allocSize <- forAll $ Gen.int (Range.linear 1 ((freeSpace (getArenaStats basicArena)) `div` 4))
  -- make three alloc requests, all should succeed
  let allocResult = chainAllocations [allocSize, allocSize, allocSize] alignment basicArena
  case allocResult of
    Just _  -> pure ()
    Nothing -> do footnote $ "allocations should not have exceeded the arena capacity"
                  failure


-- | We do several allocations that should exceed the available capacity of the arena, so an allocation should fail
prop_exceedCapacity :: Property
prop_exceedCapacity = property $ do
  let basicArena = mkSimpleMemoryArena basicArenaConfig
  -- make alloc request used half the free space
  let baseAllocSize = (freeSpace (getArenaStats basicArena)) `div` 2  
  let genAllocSize = (Gen.int (Range.linear baseAllocSize (baseAllocSize+10)))
  let alignment = MemAlign 4
  -- make three or more alloc requests, which guarantees failure
  allocSequence <- forAll $ Gen.list (Range.constant 3 6) genAllocSize
  let allocResult = chainAllocations allocSequence alignment basicArena
  case allocResult of
    Nothing -> pure ()
    Just _ -> do footnote $ "allocations should have exceeded the arena capacity"
                 failure

-- | We do several allocations, then return them in a different order. After returning all allocations there should be
--   zero used space
prop_shuffledReturn :: Property
prop_shuffledReturn = property $ do
  let basicArena = mkSimpleMemoryArena basicArenaConfig
  -- make some set of allocations
  allocationCount <- forAll $ Gen.int (Range.linear 3 10)
  let maxAllocSize = (freeSpace (getArenaStats basicArena)) `div` (allocationCount + 1)
  let genAllocSize = (Gen.int (Range.linear 1 maxAllocSize))
  let alignment = MemAlign 4
  allocSizes <- forAll $ Gen.list (Range.constant allocationCount allocationCount) genAllocSize
  let allocResult = chainAllocations allocSizes alignment basicArena
  case allocResult of
    Nothing -> do footnote $ "allocations should not have failed"
                  failure
    Just (arena', allocatedBlocks) -> do
      -- we'll return the blocks in an order different than the allocation order
      returnOrder <- forAll $ Gen.shuffle allocatedBlocks
      case chainReturns returnOrder arena' of
        Nothing -> do footnote $ "returns failed"
                      failure
        Just arena'' -> do
          let stats = getArenaStats arena''
          -- After returning all blocks, there should be one free block 100% of the time
          classify "One free block after all blocks returned (should be 100%): " (blockCount stats == 1)
          assert (usedSpace stats == 0)
