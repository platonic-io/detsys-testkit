module DisruptorTest where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.IORef
import System.IO
import System.IO.Error
import Test.HUnit
import Test.QuickCheck

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

unit_ringBufferSingleProducerSingleConsumer :: Assertion
unit_ringBufferSingleProducerSingleConsumer = do
  rb <- newRingBuffer SingleProducer 128
  counter <- newIORef 0 :: IO (IORef Int)

  let production   () = threadDelay 1000 >> atomicModifyIORef' counter (\n -> (n + 1, (n, ())))
      backPressure () = return ()
  ep <- newEventProducer rb production backPressure ()
  let handler seen n _snr endOfBatch
        | n `Set.member` seen = error (show n ++ " appears twice")
        | otherwise           = do
            putStrLn ("consumer got: " ++ show n ++
                      if endOfBatch then ". End of batch!" else "")
            return (Set.insert n seen)
  ec <- newEventConsumer rb handler Set.empty [] (Sleep 1000)

  setGatingSequences rb [Exists ec]

  let areWeDoneProducing = do
        n <- readIORef counter
        if n >= atLeastThisManyEvents
        then return ()
        else do
          threadDelay 10000
          areWeDoneProducing

      areWeDoneConsuming = do
        snr <- readIORef (ecSequenceNumber ec)
        -- NOTE: We need -1 below because the sequence number starts at 0. We
        -- don't really need it in `areWeDoneProducing`, because producing one
        -- extra event doesn't matter.
        if snr >= fromIntegral atLeastThisManyEvents - 1
        then return ()
        else do
          threadDelay 10000
          areWeDoneConsuming

  withEventProducer ep $ \aep ->
    withEventConsumer ec $ \aec ->
      withAsync areWeDoneProducing $ \ap -> do
        withAsync areWeDoneConsuming $ \ac -> do
          wait ap
          shutdownProducer ep
          wait aep
          putStrLn "Done producing!"
          wait ac
          shutdownConsumer ec
          seen <- wait aec
          putStrLn "Done consuming!"
          assert (increasingByOneFrom 0 (Set.toList seen))
  where
    atLeastThisManyEvents = 1000

    increasingByOneFrom :: Int -> [Int] -> Bool
    increasingByOneFrom n [] = n >= atLeastThisManyEvents && n < atLeastThisManyEvents + 500
    increasingByOneFrom n (i : is) | n == i    = increasingByOneFrom (n + 1) is
                                   | otherwise = False

unit_ringBufferFiveProducersOneConsumer :: Assertion
unit_ringBufferFiveProducersOneConsumer = do
  rb <- newRingBuffer MultiProducer 128
  counter <- newIORef 0 :: IO (IORef Int)

  let production   () = atomicModifyIORef' counter (\n -> (n + 1, (n, ())))
      backPressure () = return ()
  ep1 <- newEventProducer rb production backPressure ()
  ep2 <- newEventProducer rb production backPressure ()
  ep3 <- newEventProducer rb production backPressure ()
  ep4 <- newEventProducer rb production backPressure ()
  ep5 <- newEventProducer rb production backPressure ()

  let handler seen n _snr endOfBatch
        | n `Set.member` seen = error (show n ++ " appears twice")
        | otherwise           = do
            putStrLn ("consumer got: " ++ show n ++
                      if endOfBatch then ". End of batch!" else "")
            return (Set.insert n seen)
  ec <- newEventConsumer rb handler Set.empty [] (Sleep 1000)

  setGatingSequences rb [Exists ec]

  let areWeDoneProducing = do
        n <- readIORef counter
        if n >= atLeastThisManyEvents
        then return ()
        else do
          threadDelay 10
          areWeDoneProducing

      areWeDoneConsuming = do
        snr <- readIORef (ecSequenceNumber ec)
        -- NOTE: We need -1 below because the sequence number starts at 0. We
        -- don't really need it in `areWeDoneProducing`, because producing one
        -- extra event doesn't matter.
        if snr >= fromIntegral atLeastThisManyEvents - 1
        then return ()
        else do
          threadDelay 10
          areWeDoneConsuming

  withEventProducer ep1 $ \aep1 ->
    withEventProducer ep2 $ \aep2 ->
      withEventProducer ep3 $ \aep3 ->
        withEventProducer ep4 $ \aep4 ->
          withEventProducer ep5 $ \aep5 ->
            withEventConsumer ec $ \aec ->
              withAsync areWeDoneProducing $ \ap -> do
                withAsync areWeDoneConsuming $ \ac -> do
                  wait ap
                  mapM_ shutdownProducer [ep1, ep2, ep3, ep4, ep5]
                  mapM_ wait [aep1, aep2, aep3, aep4, aep5]
                  putStrLn "Done producing!"
                  wait ac
                  shutdownConsumer ec
                  seen <- wait aec
                  putStrLn "Done consuming!"
                  assertEqual "increasingByOneFrom"
                    (Right ())
                    (increasingByOneFrom 0 (Set.toList seen))
  where
    atLeastThisManyEvents = 1024

    increasingByOneFrom :: Int -> [Int] -> Either String ()
    increasingByOneFrom n []
      | n >= atLeastThisManyEvents = Right ()
      | n <  atLeastThisManyEvents =
          Left ("n (= " ++ show n ++ ") < atLeastThisManyEvents (= " ++
                show atLeastThisManyEvents ++ ")")
    increasingByOneFrom n (i : is) | n == i    = increasingByOneFrom (n + 1) is
                                   | otherwise =
      Left ("Expected: " ++ show n ++ ", but got: " ++ show i)


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
