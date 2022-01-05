module Journal.Internal.AtomicsTest where

import Foreign
import Data.Word
import Control.Concurrent
import Control.Concurrent.Async
import Test.Tasty.HUnit (Assertion, assertEqual, assertBool)

import Journal.Internal.Atomics
import Journal.Types.AtomicCounter

------------------------------------------------------------------------

assertMultiThreaded :: Assertion
assertMultiThreaded = do
  capa <- getNumCapabilities
  assertBool "The -threaded flag needs to be passed" (capa > 1)

unit_atomicFetchAdd :: Assertion
unit_atomicFetchAdd = do
  assertMultiThreaded
  let n = 10000
      c = 6
  alloca $ \ptr -> do
    poke ptr 0
    replicateConcurrently_ c (worker ptr n)
    result <- peek ptr
    assertEqual "" result (fromIntegral (c * n))
  where
    worker :: Ptr Word16 -> Int -> IO ()
    worker ptr = go
      where
        go 0 = return ()
        go n = do
          _ <- fetchAddWord16Ptr ptr 1
          go (n - 1)

unit_atomicCASSequential :: Assertion
unit_atomicCASSequential =
  alloca $ \ptr -> do
    poke ptr 0
    b <- casIntPtr ptr 0 1
    assertBool "casIntPtr 0 1" b
    b' <- casIntPtr ptr 2 3
    assertBool "casIntPtr 2 3" (not b')
    b'' <- casIntPtr ptr 1 2
    assertBool "casIntPtr 1 2" b''
    result <- peek ptr
    assertEqual "" 2 result

-- NOTE: This doesn't test possible races on writes, not sure if we can do that
-- without introducing linearisability.
unit_atomicCAS :: Assertion
unit_atomicCAS = do
  assertMultiThreaded

  let n = 10000
      c = 6

  counter <- newCounter 0

  alloca $ \ptr -> do
    poke ptr 0
    replicateConcurrently_ c (worker ptr counter n)
    result <- peek ptr
    assertEqual "" n result
  where
    worker :: Ptr Int -> AtomicCounter -> Int -> IO ()
    worker ptr counter n = do
      old <- getAndIncrCounter 1 counter
      if old < n
      then go old >> worker ptr counter n
      else return ()
      where
        go old = do
            let new = old + 1
            b <- casIntPtr ptr old new
            if b then return () else go old
