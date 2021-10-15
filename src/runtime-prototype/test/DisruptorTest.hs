module DisruptorTest where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.IORef
import System.IO
import System.IO.Error
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Disruptor

------------------------------------------------------------------------

(@?->) :: (Eq a, Show a) => IO a -> a -> Assertion
mx @?-> y = do
  x <- mx
  x @?= y

unit_ringBufferSingleNonBlocking :: Assertion
unit_ringBufferSingleNonBlocking = do
  rb <- newRingBuffer SingleProducer 8
  Just i <- tryNext rb
  set rb i 'a'
  publish rb i
  get rb i @?-> Just 'a'
  Just j <- tryNext rb
  set rb j 'b'
  publish rb j
  get rb j @?-> Just 'b'

unit_ringBufferSingleBlocking :: Assertion
unit_ringBufferSingleBlocking = do
  rb <- newRingBuffer SingleProducer 8
  i <- next rb
  set rb i 'a'
  publish rb i
  get rb i @?-> Just 'a'
  j <- next rb
  set rb j 'b'
  publish rb j
  get rb j @?-> Just 'b'

unit_ringBufferRemainingCapacity :: Assertion
unit_ringBufferRemainingCapacity = do
  rb <- newRingBuffer SingleProducer 1
  consumerSnrRef <- newIORef (SequenceNumber (-1))
  let dummy = error "never used."
  setGatingSequences rb [Exists (EventConsumer consumerSnrRef dummy () dummy)]
  remainingCapacity rb @?-> 1
  publish rb (SequenceNumber 0)
  remainingCapacity rb @?-> 0
  tryNext rb @?-> Nothing
  modifyIORef consumerSnrRef succ
  remainingCapacity rb @?-> 1

unit_ringBufferMulti :: Assertion
unit_ringBufferMulti = do
  rb <- newRingBuffer MultiProducer 8
  Just i <- tryNext rb
  set rb i 'a'
  publish rb i
  get rb i @?-> Just 'a'
  Just j <- tryNext rb
  set rb j 'b'
  publish rb j
  get rb j @?-> Just 'b'

unit_ringBufferTwoProducersOneConsumer :: Assertion
unit_ringBufferTwoProducersOneConsumer = do
  rb <- newRingBuffer SingleProducer 128
  counter <- newIORef 0 :: IO (IORef Int)
  let production   () = atomicModifyIORef' counter (\n -> (n + 1, (n, ())))
      backPressure () = return ()
  ep1 <- newEventProducer rb production backPressure ()
  ep2 <- newEventProducer rb production backPressure ()
  let handler seen n _snr endOfBatch
        | n `Set.member` seen = error (show n ++ " appears twice")
        | otherwise           = return (Set.insert n seen)
  ec <- newEventConsumer rb handler Set.empty [] (Sleep 1000)

  let areWeThereYet = areWeDoneProducing >> areWeDoneConsuming

      areWeDoneProducing = do
        n <- readIORef counter
        if n >= 10
        then return ()
        else do
          threadDelay 10000
          areWeDoneProducing

      areWeDoneConsuming = do
        snr <- readIORef (ecSequenceNumber ec)
        if snr >= fromIntegral 10
        then return ()
        else do
          threadDelay 10000
          areWeDoneConsuming

  withEventProducer ep1 $ \aep1 ->
    withEventProducer ep2 $ \aep2 ->
      withEventConsumer ec $ \aec ->
        withAsync areWeThereYet $ \a -> do
          wait a
          mapM_ shutdownProducer [ep1, ep2]
          shutdownConsumer ec
          mapM_ wait [aep1, aep2]
          seen <- wait aec
          print seen

  {-
main :: IO ()
main = do
  let pipe = "/tmp/producer-pipe"
  safeCreateNamedPipe pipe
  h <- openFile pipe ReadWriteMode
  hSetBuffering h LineBuffering
  let backPressure = const (threadDelay 1000000)
  ep <- newEventProducer rb producer backPressure h
  link (epAsync ep)
  ec <- newEventConsumer handler rb [] (Sleep 1000000)
  setGatingSequences rb [ec]
  link (ecAsync ec)
  threadDelay 5000000
  cancel (epAsync ep)
  cancel (ecAsync ec)
  return ()
  where
    handler str snr eob = putStrLn (show (getSequenceNumber snr) ++ ": " ++ str ++
                                    if eob then ";" else "")
    producer h = do
      l <- hGetLine h
      return (l, h)

safeCreateNamedPipe :: FilePath -> IO ()
safeCreateNamedPipe fp =
  catchJust
    (\e -> if isAlreadyExistsErrorType (ioeGetErrorType e)
           then Just ()
           else Nothing)
    (createNamedPipe fp
      (namedPipeMode `unionFileModes`
       ownerReadMode `unionFileModes`
       ownerWriteMode))
    return

-}
