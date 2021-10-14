module DisruptorTest where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.HUnit
import Data.IORef

import Disruptor

------------------------------------------------------------------------

(@?=>) :: (Eq a, Show a) => IO a -> a -> Assertion
mx @?=> y = do
  x <- mx
  x @?= y

unit_ringBufferSingleNonBlocking :: Assertion
unit_ringBufferSingleNonBlocking = do
  rb <- newRingBuffer SingleProducer 8
  Just i <- tryNext rb
  set rb i 'a'
  publish rb i
  get rb i @?=> Just 'a'
  Just j <- tryNext rb
  set rb j 'b'
  publish rb j
  get rb j @?=> Just 'b'

unit_ringBufferSingleBlocking :: Assertion
unit_ringBufferSingleBlocking = do
  rb <- newRingBuffer SingleProducer 8
  i <- next rb
  set rb i 'a'
  publish rb i
  get rb i @?=> Just 'a'
  j <- next rb
  set rb j 'b'
  publish rb j
  get rb j @?=> Just 'b'

unit_ringBufferRemainingCapacity :: Assertion
unit_ringBufferRemainingCapacity = do
  rb <- newRingBuffer SingleProducer 1
  consumerSnrRef <- newIORef (SequenceNumber (-1))
  let dummyAsync = error "never used."
  setGatingSequences rb [EventConsumer consumerSnrRef dummyAsync]
  remainingCapacity rb @?=> 1
  publish rb (SequenceNumber 0)
  remainingCapacity rb @?=> 0
  tryNext rb @?=> Nothing
  modifyIORef consumerSnrRef succ
  remainingCapacity rb @?=> 1

unit_ringBufferMulti :: Assertion
unit_ringBufferMulti = do
  rb <- newRingBuffer MultiProducer 8
  Just i <- tryNext rb
  set rb i 'a'
  publish rb i
  get rb i @?=> Just 'a'
  Just j <- tryNext rb
  set rb j 'b'
  publish rb j
  get rb j @?=> Just 'b'
