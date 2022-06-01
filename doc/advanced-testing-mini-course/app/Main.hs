module Main where

import Control.Exception (assert)
import Control.Monad (unless)
import System.Environment
import System.Exit (die)

import Lec05.ClientGenerator
import Lec05.ErrorReporter
import Lec05.EventLoop
import Lec05.StateMachine
import Lec05.Configuration
import Lec05.Codec
import Lec05.Debug
import Lec05.Deployment
import Lec05.Network
import Lec05.Random
import Lec05.History
import Lec05.Time

import qualified Lec04.LineariseWithFault as Lec4

import Lec05.ViewstampReplication.State (ReplicatedStateMachine(..))
import qualified Lec05.ViewstampReplication.Machine as VR
import Lec05.ViewstampReplication.Message

------------------------------------------------------------------------

runMany :: [Seed] -> (Seed -> IO Bool) -> IO ()
runMany origxs f = go (1 :: Int) origxs
  where
  go _i [] = return ()
  go i (x:xs) = do
    putStrLn $ "Running iteration: " ++ show i ++ " Seed " ++ show (unSeed x)
    res <- f x
    if res
      then go (succ i) xs
      else do
        die $ "Failed iteration: " ++ show i ++ " : Seed " ++ show (unSeed x)

smI :: ReplicatedStateMachine [String] String [String]
smI = ReplicatedStateMachine $ \ s o -> (o:s, o:s)

type Model = [String]
type Command = VRRequest String
type Response = VRResponse [String]

step :: Model -> Command -> (Model, Response)
step xs (VRRequest op rn) = (op:xs, VRReply 0 rn (op:xs))

initModel :: [String]
initModel = []

markFailure :: Lec4.History' Command Response -> Lec4.History' Command Response
markFailure (Lec4.History ops) = Lec4.History (finishClients [] $ map go ops)
  where
    go i@Lec4.Invoke{} = i
    go f@Lec4.Fail{} = f
    go (Lec4.Ok p VROnlyOneInflightAllowed{}) = Lec4.Fail p Lec4.FAIL
    go o@Lec4.Ok{} = o

    remove x = filter (/= x)

    finishClients ps [] = [ Lec4.Fail p Lec4.FAIL | p <- ps]
    finishClients ps (op:h) = case op of
      Lec4.Invoke p _ -> op : finishClients (p:ps) h
      Lec4.Ok p _ -> op : finishClients (remove p ps) h
      Lec4.Fail p _ -> op : finishClients (remove p ps) h

vrClientGenerator :: SingleStateGenerator
vrClientGenerator = SingleStateGenerator
  0
  (+1)
  (\ curRequestNumber ->
     let msg = "msg" ++ show curRequestNumber
     in (NodeId 0, encShow $ VRRequest msg curRequestNumber))

vrClientDelay :: NominalDiffTime
vrClientDelay = 2

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--simulation"] -> do
      h <- newHistory
      _collector <- eventLoopSimulation (Seed 0) echoAgenda h [SomeCodecSM idCodec echoSM]
      _history <- readHistory h
      putStrLn "Can't print history yet, need Show/Pretty constraints for parameters..."
    ["vr", "--simulation", seed, fp] -> do
      h <- newHistory
      let
        nodes = map NodeId [0..4]
        delta = 15 -- time for primary to do re-broadcast
        vrSM me = VR.vrSM (filter (/= me) nodes) me delta [] smI
        printItem label prefix thing =
          putStrLn $ "\x1b[33m" <> label <> ":\x1b[0m " <> prefix <> show thing
        printE (HistEvent' d (HistEvent n bs inp as msgs)) = do
          putStrLn "\n\x1b[32mNew Entry\x1b[0m"
          printItem "Node" " " n
          printItem "State before" "\n" bs
          printItem "Input" (case d of { DidDrop -> "\x1b[31m[DROPPED]\x1b[0m "; DidArrive -> " "}) inp
          printItem "State after" "\n" as
          printItem "Sent messages" "" ""
          mapM_ (\x -> putStrLn $ "  " <> show x) msgs
        fs = FailureSpec (NetworkFaults 0.15)
        seed' = read seed
        endTime = addTimeSeconds 3600 epoch
      collector <- eventLoopFaultySimulation (Seed seed') (VR.agenda endTime) h fs
        [ SomeCodecSM VR.vrCodec (vrSM me) | me <- nodes] (Just (vrClientGenerator, vrClientDelay))
      history <- readHistory h
      mapM_ printE history
      -- let's print the errors again so they are easier to see.
      reportedErrors <- readFromCollector collector
      unless (null reportedErrors) (putStrLn "")
      mapM_ putStrLn reportedErrors
      writeDebugFile fp history
      let bbHistory = markFailure (blackboxFailHistory (fmap heEvent history))
      assert (Lec4.linearise step initModel bbHistory) (return ())
    ["vr", "--simulation-explore"] -> do
      seeds <- generateSeeds 10
      runMany seeds $ \ seed -> do
        h <- newHistory
        let
          nodes = map NodeId [0..4]
          delta = 15 -- time for primary to do re-broadcast
          vrSM me = VR.vrSM (filter (/= me) nodes) me delta [] smI
          fs = FailureSpec (NetworkFaults 0.15)
          endTime = addTimeSeconds 3600 epoch
        collector <- eventLoopFaultySimulation seed (VR.agenda endTime) h fs
          [ SomeCodecSM VR.vrCodec (vrSM me) | me <- nodes] (Just (vrClientGenerator, vrClientDelay))
        history <- readHistory h
        -- let's print the errors again so they are easier to see.
        reportedErrors <- readFromCollector collector
        unless (null reportedErrors) (putStrLn "")
        mapM_ putStrLn reportedErrors
        let bbHistory = markFailure (blackboxFailHistory (fmap heEvent history))
            isValid = Lec4.linearise step initModel bbHistory
        print bbHistory
        return (null reportedErrors && isValid)
    _otherwise       -> eventLoopProduction [SomeCodecSM idCodec echoSM]
