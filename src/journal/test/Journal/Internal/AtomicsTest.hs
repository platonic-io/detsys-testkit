module Journal.Internal.AtomicsTest where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad(unless)
import Data.IORef
import Data.List (sort)
import Data.Word
import Foreign
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual)
import Test.QuickCheck
import Test.QuickCheck.Monadic

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
    b <- casInt32Ptr ptr 0 1
    assertBool "casIntPtr 0 1" b
    b' <- casInt32Ptr ptr 2 3
    assertBool "casIntPtr 2 3" (not b')
    b'' <- casInt32Ptr ptr 1 2
    assertBool "casIntPtr 1 2" b''
    result <- peek ptr
    assertEqual "" 2 result

unit_atomicCASConcurrent :: Assertion
unit_atomicCASConcurrent = do
  assertMultiThreaded

  let n = 10000
      c = 6

  counter <- newCounter 0

  alloca $ \ptr -> do
    poke ptr 0
    histRef <- newIORef []
    replicateConcurrently_ c (worker ptr counter histRef n)
    hist <- readIORef histRef
    result <- peek ptr
    assertEqual "" [0..n] (sort (result : hist))
  where
    worker :: Ptr Int -> AtomicCounter -> IORef [Int] -> Int -> IO ()
    worker ptr counter hist n = do
      ticket <- incrCounter 1 counter
      if ticket <= n
      then go ticket >> worker ptr counter hist n
      else return ()
      where
        go :: Int -> IO ()
        go ticket = do
          current <- peek ptr
          b <- casIntPtr ptr current ticket
          if b
          then atomicModifyIORef' hist (\hist -> ((current : hist), ()))
          else go ticket

prop_cas32Prop :: Int32 -> Int32 -> Property
prop_cas32Prop old new = monadicIO $ do
  p <- run malloc
  run $ poke p old
  v <- run $ peek p
  assertEq v old
  success <- run $ casInt32Ptr p old new
  assertBool success "cas did not succeed"
  v' <- run $ peek p
  assertEq v' new
  where
    assertBool condition msg = do
      unless condition $
        monitor (counterexample $ "Failed: " <> msg)
      assert condition
    assertEq x y = do
      unless (x == y) $
        monitor (counterexample $ "Failed: " <> show x <> " is not equal to " <> show y)
      assert (x == y)

prop_cas64Prop :: Int64 -> Int64 -> Property
prop_cas64Prop old new = monadicIO $ do
  p <- run malloc
  run $ poke p old
  v <- run $ peek p
  assertEq v old
  success <- run $ casInt64Ptr p old new
  assertBool success "cas did not succeed"
  v' <- run $ peek p
  assertEq v' new
  where
    assertBool condition msg = do
      unless condition $
        monitor (counterexample $ "Failed: " <> msg)
      assert condition
    assertEq x y = do
      unless (x == y) $
        monitor (counterexample $ "Failed: " <> show x <> " is not equal to " <> show y)
      assert (x == y)
