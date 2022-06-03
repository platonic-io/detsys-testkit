module Lec05.TimerWheel where

import Data.Time
import Data.Fixed
import Data.IORef
import Data.Heap (Entry(Entry), Heap)
import qualified Data.Heap as Heap

import Lec05.Time
import Lec05.Event
import Lec05.StateMachine

------------------------------------------------------------------------

newtype TimerWheel = TimerWheel (IORef (Heap (Entry Time (NodeId, TimerId))))

newTimerWheel :: IO TimerWheel
newTimerWheel = do
  ref <- newIORef Heap.empty
  return (TimerWheel ref)

registerTimer :: TimerWheel -> Clock -> NodeId -> TimerId -> Pico -> IO ()
registerTimer (TimerWheel hr) clock nodeId timerId secs = do
  now <- cGetCurrentTime clock
  let expires = addTimeSeconds (secondsToNominalDiffTime secs) now
  atomicModifyIORef' hr (\h -> (Heap.insert (Entry expires (nodeId, timerId)) h, ()))

resetTimer :: TimerWheel -> Clock -> NodeId -> TimerId -> Pico -> IO ()
resetTimer (TimerWheel hr) clock nodeId timerId secs = do
  now <- cGetCurrentTime clock
  let expires = addTimeSeconds (secondsToNominalDiffTime secs) now
  atomicModifyIORef' hr (\h -> (Heap.map (go expires) h, ()))
    where
      entry = (nodeId, timerId)

      go expires e@(Entry _expires entry')
        | entry' == entry = Entry expires entry'
        | otherwise       = e

cancelTimer :: TimerWheel -> NodeId -> TimerId -> IO ()
cancelTimer (TimerWheel hr) nodeId timerId =
  atomicModifyIORef' hr (\h -> (Heap.filter ((/= (nodeId, timerId)) . Heap.payload) h , ()))

popTimer :: TimerWheel -> IO ()
popTimer (TimerWheel hr) =  atomicModifyIORef' hr (\h -> (Heap.deleteMin h, ()))

data NextTimer = None | Now TimerEvent | Later Int TimerEvent

nextTimer :: TimerWheel -> Clock -> IO NextTimer
nextTimer (TimerWheel hr) clock = do
  now <- cGetCurrentTime clock
  heap <- readIORef hr
  case Heap.viewMin heap of
    Nothing -> return None
    Just (Entry expires (nodeId, timerId), _heap')
      | expires <= now -> return (Now   (TimerEvent nodeId timerId expires))
      | otherwise      -> return (Later (diffTimeMicros expires now)
                                        (TimerEvent nodeId timerId expires))
