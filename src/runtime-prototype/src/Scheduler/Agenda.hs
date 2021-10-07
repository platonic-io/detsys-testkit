module Scheduler.Agenda where

import Data.Heap (Entry(Entry), Heap)
import qualified Data.Heap as Heap

import Scheduler.Event
import StuntDouble.Time

------------------------------------------------------------------------

newtype Agenda = Agenda (Heap (Entry Time SchedulerEvent))

empty :: Agenda
empty = Agenda Heap.empty

union :: Agenda -> Agenda -> Agenda
union (Agenda h) (Agenda h') = Agenda (h `Heap.union` h')

fromList :: [(Time, SchedulerEvent)] -> Agenda
fromList = Agenda . Heap.fromList . map (uncurry Entry)

pop :: Agenda -> Maybe ((Time, SchedulerEvent), Agenda)
pop (Agenda h) = case Heap.uncons h of
  Nothing              -> Nothing
  Just (Entry t e, h') -> Just ((t, e), Agenda h')

push :: (Time, SchedulerEvent) -> Agenda -> Agenda
push (t, e) (Agenda h) = Agenda (Heap.insert (Entry t e) h)
