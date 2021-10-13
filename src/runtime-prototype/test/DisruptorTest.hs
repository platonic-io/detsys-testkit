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

unit_ringBufferSingle :: Assertion
unit_ringBufferSingle = do
  rb <- newRingBuffer SingleProducer 8
  Just i <- tryNext rb
  set rb i 'a'
  publish rb i
  get rb i @?=> Just 'a'
  Just j <- tryNext rb
  set rb j 'b'
  publish rb j
  get rb j @?=> Just 'b'

unit_ringBufferRemainingCapacity :: Assertion
unit_ringBufferRemainingCapacity = do
  rb <- newRingBuffer SingleProducer 1
  snr <- newIORef (SequenceNumber (-1))
  let dummyAsync = error "never used."
  setGatingSequences rb [EventConsumer snr dummyAsync]
  remainingCapacity rb @?=> 1
  publish rb (SequenceNumber 0)
  remainingCapacity rb @?=> 0
  tryNext rb @?=> Nothing

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
