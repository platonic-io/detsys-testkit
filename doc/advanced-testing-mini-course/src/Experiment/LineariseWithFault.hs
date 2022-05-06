{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Experiment.LineariseWithFault where

import Data.Tree (Forest, Tree(Node))

import Lec01SMTesting
import qualified Lec02ConcurrentSMTesting as Lec2

newtype History' cmd resp = History [Operation' cmd resp]
  deriving (Show, Functor, Foldable)

type History = History' Command Response

data FailureMode
  = FAIL
  | INFO
  deriving Show

data Operation' cmd resp
  = Invoke Lec2.Pid cmd
  | Ok     Lec2.Pid resp
  | Fail   Lec2.Pid FailureMode
  deriving (Show, Functor, Foldable)

type Operation = Operation' Command Response

data Result resp
  = OkWithResponse resp
  | OkWithNoResponse
  deriving Show

interleavings :: History' cmd resp -> Forest (cmd, Result resp)
interleavings (History [])  = []
interleavings (History ops) | all (not . isOk) ops = []
  where
    isOk :: Operation' cmd resp -> Bool
    isOk (Ok{}) = True
    isOk _ = False
interleavings (History ops0) =
  [ Node (cmd, resp) (interleavings (History ops'))
  | (tid, cmd)   <- takeInvocations ops0
  , (resp, ops') <- findResponse tid
                      (filter1 (not . matchInvocation tid) ops0)
  ]
  where
    takeInvocations :: [Operation' cmd resp] -> [(Lec2.Pid, cmd)]
    takeInvocations []                         = []
    takeInvocations ((Invoke pid cmd)   : ops) = (pid, cmd) : takeInvocations ops
    takeInvocations ((Ok    _pid _resp) : _)   = []
    takeInvocations ((Fail _pid _mode) : ops)  = takeInvocations ops

    findResponse :: Lec2.Pid -> [Operation' cmd resp] -> [(Result resp, [Operation' cmd resp])]
    findResponse _pid []                                   = []
    findResponse  pid ((Ok pid' resp) : ops) | pid == pid' = [(OkWithResponse resp, ops)]
    findResponse  pid ((Fail pid' mode) : ops)
      | pid == pid' = case mode of
          FAIL -> []
          INFO -> [(OkWithNoResponse, ops)]
    findResponse  pid (op             : ops)               =
      [ (resp, op : ops') | (resp, ops') <- findResponse pid ops ]

    matchInvocation :: Lec2.Pid -> Operation' cmd resp -> Bool
    matchInvocation pid (Invoke pid' _cmd) = pid == pid'
    matchInvocation _   _                  = False

    filter1 :: (a -> Bool) -> [a] -> [a]
    filter1 _ []                   = []
    filter1 p (x : xs) | p x       = x : filter1 p xs
                       | otherwise = xs

linearisable :: forall model cmd resp. Eq resp
             => (model -> cmd -> (model, resp)) -> model -> Forest (cmd, Result resp) -> Bool
linearisable step0 model0 = any' (go model0)
  where
    go :: model -> Tree (cmd, Result resp) -> Bool
    go model (Node (cmd, mresp) ts) =
      let
        (model', resp') = step0 model cmd
      in case mresp of
        OkWithResponse resp -> resp == resp' && any' (go model') ts
        OkWithNoResponse -> any' (go model') ts

    any' :: (a -> Bool) -> [a] -> Bool
    any' _p [] = True
    any'  p xs = any p xs

example :: History' String String
example = History
  [ Invoke p0 "A"
  , Invoke p2 "B"
  , Invoke p1 "C"
  , Ok p0 "RA"
  , Fail p2 INFO
  , Invoke p0 "D"
  , Ok p1 "RC"
  , Ok p0 "RD"
  ]
  where
    p0 = Lec2.Pid 0
    p1 = Lec2.Pid 1
    p2 = Lec2.Pid 2
