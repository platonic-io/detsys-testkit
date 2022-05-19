module Lec05.TimerWheel where

import Data.Time
import Data.Fixed
import Data.Foldable
import Data.IORef
import Data.Heap (Entry(Entry), Heap)
import qualified Data.Heap as Heap

import Lec05.Time
import Lec05.Event
import Lec05.EventQueue
import Lec05.StateMachine

------------------------------------------------------------------------

newtype TimerWheel = TimerWheel (IORef (Heap (Entry Time NodeId)))

newTimerWheel :: IO TimerWheel
newTimerWheel = do
  ref <- newIORef Heap.empty
  return (TimerWheel ref)

registerTimer :: TimerWheel -> Clock -> NodeId -> Pico -> IO ()
registerTimer (TimerWheel hr) clock nodeId secs = do
  now <- cGetCurrentTime clock
  let expires = addTime (secondsToNominalDiffTime secs) now
  atomicModifyIORef' hr (\h -> (Heap.insert (Entry expires nodeId) h, ()))

resetTimer :: TimerWheel -> Clock -> NodeId -> Pico -> IO ()
resetTimer (TimerWheel hr) clock nodeId secs = do
  now <- cGetCurrentTime clock
  let expires = addTime (secondsToNominalDiffTime secs) now
  atomicModifyIORef' hr (\h -> (Heap.map (go expires) h, ()))
    where
      go expires e@(Entry _expires nodeId')
        | nodeId' == nodeId = Entry expires nodeId
        | otherwise         = e

popTimer :: TimerWheel -> IO ()
popTimer (TimerWheel hr) =  atomicModifyIORef' hr (\h -> (Heap.deleteMin h, ()))

data NextTimer = None | Now TimerEvent | Later Int TimerEvent

nextTimer :: TimerWheel -> Clock -> IO NextTimer
nextTimer (TimerWheel hr) clock = do
  now <- cGetCurrentTime clock
  heap <- readIORef hr
  case Heap.viewMin heap of
    Nothing -> return None
    Just (Entry expires nodeId, _heap')
      | expires <= now -> return (Now   (TimerEvent nodeId expires))
      | otherwise      -> return (Later (diffTimeMicros expires now)
                                        (TimerEvent nodeId expires))
