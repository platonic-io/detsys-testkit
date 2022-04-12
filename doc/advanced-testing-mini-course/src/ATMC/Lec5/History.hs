{-# LANGUAGE ExistentialQuantification #-}

module ATMC.Lec5.History where

import ATMC.Lec5.StateMachine

------------------------------------------------------------------------

data HistEvent = forall state req msg resp.
  HistEvent NodeId state (Input req msg) state [Output resp msg]
