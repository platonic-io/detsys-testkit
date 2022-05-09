{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lec04.LineariseWithFault where

import Data.Tree (Forest, Tree(Node))
import Test.QuickCheck hiding (Result)

import Lec01SMTesting
import Lec02ConcurrentSMTesting (Pid(..))

newtype History' cmd resp = History [Operation' cmd resp]
  deriving (Show, Functor, Foldable)

type History = History' Command Response

data FailureMode
  = FAIL
  | INFO
  deriving Show

-- XXX: We probably don't want to use Pid = ThreadId here, because if we have an
-- INFO we don't want any further operations executed by that Pid.
data Operation' cmd resp
  = Invoke Pid cmd
  | Ok     Pid resp
  | Fail   Pid FailureMode -- should this has a cmd?
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
    takeInvocations :: [Operation' cmd resp] -> [(Pid, cmd)]
    takeInvocations []                         = []
    takeInvocations ((Invoke pid cmd)   : ops) = (pid, cmd) : takeInvocations ops
    takeInvocations ((Ok    _pid _resp) : _)   = []
    takeInvocations ((Fail _pid _mode) : ops)  = takeInvocations ops

    findResponse :: Pid -> [Operation' cmd resp] -> [(Result resp, [Operation' cmd resp])]
    findResponse _pid []                                   = []
    findResponse  pid ((Ok pid' resp) : ops) | pid == pid' = [(OkWithResponse resp, ops)]
    findResponse  pid ((Fail pid' mode) : ops)
      | pid == pid' = case mode of
          FAIL -> []
          INFO -> [(OkWithNoResponse, ops)]
    findResponse  pid (op             : ops)               =
      [ (resp, op : ops') | (resp, ops') <- findResponse pid ops ]

    matchInvocation :: Pid -> Operation' cmd resp -> Bool
    matchInvocation pid (Invoke pid' _cmd) = pid == pid'
    matchInvocation _   _                  = False

    filter1 :: (a -> Bool) -> [a] -> [a]
    filter1 _ []                   = []
    filter1 p (x : xs) | p x       = x : filter1 p xs
                       | otherwise = xs

linearisable :: forall model cmd resp. Eq resp => Show cmd => Show resp
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

linearise :: forall model cmd resp. Eq resp => Show cmd => Show resp
             => (model -> cmd -> (model, resp)) -> model -> History' cmd resp -> Bool
linearise step0 model0 = linearisable step0 model0 . interleavings

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

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
    p0 = Pid 0
    p1 = Pid 1
    p2 = Pid 2

--------------------------------------------------------------------------------
-- QuickCheck
--------------------------------------------------------------------------------

data LinearEvent cmd resp = LinearEvent Pid cmd resp
  deriving Show

data PidStatus cmd resp
  = DoingNothing
  | MadeRequest cmd
  | CommitedRequest cmd (Maybe resp)
  | FailedRequest cmd

-- selectOne will pick a random element, and also give the remaining elements
-- could use NonEmpty but error is fine for now
selectOne :: [x] -> Gen (x,[x])
selectOne [] = error "selectOne requires at least one element"
selectOne xs = do
  i <- chooseInt (0, length xs - 1)
  -- we could have a specialised function for this that would have better performance
  return (xs !! i, take i xs ++ drop (succ i) xs)

genHistory :: forall model cmd resp.
  (model -> cmd -> (model, resp)) -> model -> Gen cmd
  -> Int -> [Pid] -> Gen (History' cmd resp, [LinearEvent cmd resp])
genHistory step0 model0 genC nrOfNewPids pids0 = sized $ go [] [] model0 (zip pids0 $ repeat DoingNothing) nextPid0
  where
    nextPid0 = nextPid (maximum pids0)
    lastPid = foldr (.) id (replicate nrOfNewPids nextPid) $ nextPid0
    nextPid (Pid p) = Pid (succ p)
    go1 :: [Operation' cmd resp] -> [LinearEvent cmd resp] -> model
      -> Pid -> PidStatus cmd resp -> Bool
      -> Gen ( [Operation' cmd resp]
             , [LinearEvent cmd resp]
             , model
             , Maybe (PidStatus cmd resp)
             , Bool)
    go1 conc linear model pid state shouldStartNew = case state of
      DoingNothing
        | shouldStartNew -> do
          cmd <- genC
          return (Invoke pid cmd:conc, linear, model, Just $ MadeRequest cmd, True)
        | otherwise -> return (conc, linear, model, Nothing, False)
      MadeRequest cmd -> frequency
          [ (10, do -- request succeed, and response arrived
                let (model', resp) = step0 model cmd
                return (conc, LinearEvent pid cmd resp:linear, model', Just $ CommitedRequest cmd (Just resp), False)
             )
          , (1, do -- request succeed, but response failed
                let (model', resp) = step0 model cmd
                return (conc, LinearEvent pid cmd resp:linear, model', Just $ CommitedRequest cmd Nothing, False)
            )
          , (1, do -- request fails
                return (conc, linear, model, Just $ FailedRequest cmd, False)
            )]
      CommitedRequest _cmd mresp -> do
        let op = case mresp of
              Nothing -> Fail pid INFO
              Just resp-> Ok pid resp
        return (op:conc, linear, model, fmap (const DoingNothing) mresp, False)
      FailedRequest _cmd ->
        return (Fail pid INFO:conc, linear, model, Nothing, False)

    go conc linear _model []   _npid _size = pure (History $ reverse conc, reverse linear)
    go conc linear  model pids  npid  size = do
      ((pid, state), pids') <- selectOne pids
      (conc', linear', model', status, shouldChangeSize) <- go1 conc linear model pid state (0 < size)
      let (pids'', npid') = case status of
            Nothing
              | size <= 0 || npid >= lastPid -> (pids', npid)
              | otherwise -> ((npid, DoingNothing):pids', nextPid npid)
            Just s -> ((pid,s):pids', npid)
          size' = if shouldChangeSize then pred size else size
      go conc' linear' model' pids'' npid' size'

type SimpleModel = [Int]

smStep :: SimpleModel -> Int -> (SimpleModel, [Int])
smStep xs x = (x:xs, x:xs)

smModel :: SimpleModel
smModel = []

prop_Linearise :: [Pid] -> Int -> Property
prop_Linearise pids nrOfNewPids =
  forAll (genHistory smStep smModel arbitrary nrOfNewPids pids) $ \(ch, _) ->
    linearise smStep smModel ch
