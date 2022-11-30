{-# language RankNTypes #-}
module Scheduler.Agenda where

import Data.Heap (Heap)
import qualified Data.Heap as Heap

import Scheduler.Event (Event)
import Scheduler.Time (Timestamp, Duration)
import qualified Scheduler.Time as Time

type Agenda = Heap AgendaEntry

type AgendaEntry = Heap.Entry Timestamp Event

empty :: Agenda
empty = Heap.empty

mkEntry :: Timestamp -> Event -> AgendaEntry
mkEntry = Heap.Entry

theTime :: AgendaEntry -> Timestamp
theTime = Heap.priority

theEvent :: AgendaEntry -> Event
theEvent = Heap.payload

updateTime :: AgendaEntry -> Duration -> AgendaEntry
updateTime ae dur = Heap.Entry (Time.timeAdd (theTime ae) dur) (theEvent ae)

data Capability m = Capability
  { addEntries :: [AgendaEntry] -> m ()
  , peek :: m (Maybe AgendaEntry)
  , pop :: m (Maybe AgendaEntry)
  }

fromModify :: Monad m => (forall b. (Agenda -> (Agenda, b)) -> m b) -> Capability m
fromModify modify = Capability
  { addEntries = \ entries -> modify $ \ agenda -> (foldr Heap.insert agenda entries, ())
  , peek = modify $ \agenda -> (agenda, fst <$> Heap.uncons agenda)
  , pop = modify $ \agenda -> case Heap.uncons agenda of
      Nothing -> (agenda, Nothing)
      Just (el, agenda') -> (agenda', Just el)
  }
