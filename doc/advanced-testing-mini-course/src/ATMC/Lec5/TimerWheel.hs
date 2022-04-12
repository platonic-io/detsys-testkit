module ATMC.Lec5.TimerWheel where

import Data.Time
import Data.Fixed
import Data.Foldable
import Data.IORef
import Data.Heap (Entry(Entry), Heap)
import qualified Data.Heap as Heap

import ATMC.Lec5.Time
import ATMC.Lec5.StateMachine

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

expiredTimers :: TimerWheel -> Clock -> IO [(Time, NodeId)]
expiredTimers (TimerWheel hr) clock  = do
  h <- readIORef hr
  now <- cGetCurrentTime clock
  return (go (toList h) now [])
    where
      go :: [Entry Time NodeId] -> Time -> [(Time, NodeId)] -> [(Time, NodeId)]
      go []                   _now acc             = reverse acc
      go (Entry t nodeId : es) now acc | t <= now  = go es now ((t, nodeId) : acc)
                                       | otherwise = reverse acc
