module Scheduler.Agenda where

import Data.Heap (Entry(Entry), Heap)
import qualified Data.Heap as Heap
import Data.Time

import Scheduler.Event

------------------------------------------------------------------------

newtype Agenda = Agenda (Heap (Entry UTCTime SchedulerEvent))

empty :: Agenda
empty = Agenda Heap.empty

union :: Agenda -> Agenda -> Agenda
union (Agenda h) (Agenda h') = Agenda (h `Heap.union` h')

fromList :: [(UTCTime, SchedulerEvent)] -> Agenda
fromList = Agenda . Heap.fromList . map (uncurry Entry)

pop :: Agenda -> Maybe ((UTCTime, SchedulerEvent), Agenda)
pop (Agenda h) = case Heap.uncons h of
  Nothing            -> Nothing
  Just (Entry t e, h') -> Just ((t, e), Agenda h')
