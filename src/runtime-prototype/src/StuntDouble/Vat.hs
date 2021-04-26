{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StuntDouble.Vat where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Exception
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent
import Control.Monad
import Control.Monad.Free

import StuntDouble.Actor
import StuntDouble.Message
import StuntDouble.Reference

------------------------------------------------------------------------

newtype RequestId = RequestId Int
  deriving (Enum, Show, Read)

getRequestId :: RequestId -> Int
getRequestId (RequestId rid) = rid

data Vat = Vat
  { vatId       :: VatId
  , vatThreadId :: ThreadId
  , vatVatState :: VatState
  }

newtype VatId = VatId String
  deriving Eq

data VatState = VatState
  { vatStateActors        :: TVar (IntMap (Message -> Actor))
  , vatStateResponses     :: TVar (IntMap (TMVar Message))
  , vatStateNextRequestId :: TVar RequestId
  }

------------------------------------------------------------------------

data LocalRefNotOnVat = LocalRefNotOnVat
  deriving Show

instance Exception LocalRefNotOnVat

lookupActor :: VatState -> LocalRef -> STM (Message -> Actor)
lookupActor vatState (LocalRef n) = do
  actorMap <- readTVar (vatStateActors vatState)
  case IntMap.lookup n actorMap of
    Nothing -> throw LocalRefNotOnVat
    Just actor -> return actor

------------------------------------------------------------------------

data Response = Response RequestId Message
  deriving (Show, Read)

data Channel = Channel
  { channelSend    :: RemoteRef -> Response -> IO ()
  , channelReceive :: IO Response
  }

stdioChannel :: Channel
stdioChannel = Channel (\_rref msg -> print msg) (read <$> getLine)
-- ^ Send the following on stdin to respond:
--     Response (RequestId 0) (Message "foo1")
--     Response (RequestId 1) (Message "foo2")

namedPipeChannel :: FilePath -> IO Channel
namedPipeChannel fp = do
  undefined

stmChannel :: IO (Channel, Channel)
stmChannel = do
  aToB <- newEmptyTMVarIO
  bToA <- newEmptyTMVarIO
  return (Channel
          { channelSend = \_remoteRef msg -> atomically (putTMVar aToB msg)
          , channelReceive = atomically (takeTMVar bToA)
          },
           Channel
          { channelSend = \_remoteRef msg -> atomically (putTMVar bToA msg)
          , channelReceive = atomically (takeTMVar aToB)
          })

makeVat :: Channel -> IO Vat
makeVat channel = do
  actorsTVar    <- newTVarIO IntMap.empty
  responsesTVar <- newTVarIO IntMap.empty
  nextRequestIdTVar <- newTVarIO (RequestId 0)

  let vatState = VatState
                 { vatStateActors = actorsTVar
                 , vatStateResponses = responsesTVar
                 , vatStateNextRequestId = nextRequestIdTVar
                 }
  -- We want to spawn several threads here:
  --   1. That listens for external replies, like `tid` below;
  --   2. That listens for remote send requests, created by `makeRemoteCall`;
  --   3. That polls ongoing asyncs and tells the main event loop that the
  --      handler is ready to be called once the async finishes;
  --   4. The main event loop, listens for events from the other threads on some
  --      stm channel.

  tid <- forkIO $ forever $ do
    Response rid msg <- channelReceive channel
    -- XXX: handle `send`s here also?
    atomically $ do
      responses <- readTVar responsesTVar
      let actorId = 0 -- XXX: should be part of response?
      actor <- fmap (IntMap.! actorId) (readTVar actorsTVar)
      cont <- runActor vatState (actor msg)
      case cont of
        Now resp -> putTMVar (responses IntMap.! getRequestId rid) resp
        Later a h -> undefined
        -- ^ XXX: register that `h` should be called once `a` finishes somewhere
        -- in the VatState.

  return Vat
    { vatId = VatId (show tid) -- XXX: unused? perhaps useful for showing?
    , vatThreadId = tid
    , vatVatState = vatState
    }

makeRemoteCall :: VatState -> RemoteRef -> Message -> STM (Async Message)
makeRemoteCall vatState rref msg = do
  undefined

processRemoteCalls :: VatState -> Channel -> IO ()
processRemoteCalls vs channel = do
  (msg, remoteRef) <- undefined
  (requestId, responseVar) <- atomically $ do
    requestId <- readTVar (vatStateNextRequestId vs)
    modifyTVar (vatStateNextRequestId vs) succ
    responseVar <- newEmptyTMVar
    modifyTVar (vatStateResponses vs) (IntMap.insert (getRequestId requestId) responseVar)
    return (requestId, responseVar)
  channelSend channel remoteRef (Response requestId msg)

  -- XXX: putTMVar?
  undefined $ async $ atomically $ do
    resp <- takeTMVar responseVar -- XXX: timeout?
    modifyTVar (vatStateResponses vs) (IntMap.delete (getRequestId requestId))
    return resp

runActor :: VatState -> Free ActorF a -> STM a
runActor vatState actor = iterM go actor
  where
    go :: ActorF (STM a) -> STM a
    go (Call lref msg k) = do
      callee <- lookupActor vatState lref
      cont <- runActor vatState (callee msg)
      case cont of
        Now resp -> k resp
        Later a h -> undefined

    go (RemoteCall rref msg k) = do
      k =<< makeRemoteCall vatState rref msg
    go (Get k) = undefined
    go (Put state' k) = undefined


-- XXX: remoteSpawn? spawnLink?
spawn :: Vat -> (Message -> Actor) -> IO LocalRef
spawn vat actor = atomically $ do
  actors <- readTVar (vatStateActors (vatVatState vat))
  let size = IntMap.size actors -- XXX: use monotonically increasing counter or
                                -- even better: some unguessable magic cookie
  writeTVar (vatStateActors (vatVatState vat)) (IntMap.insert size actor actors)
  return (LocalRef size)

call :: Vat -> LocalRef -> Message -> IO Message
call vat localRef msg = atomically $
  runActor (vatVatState vat) (Free (Call localRef msg return))

send :: Vat -> Channel -> RemoteRef -> Message -> IO (Async Message)
send vat channel remoteRef msg = atomically $
  runActor (vatVatState vat) (Free (RemoteCall remoteRef msg return))

on :: IO (Async Message) -> (Message -> IO ()) -> IO ()
on io k = void $ forkIO $ do
  a <- io
  msg <- wait a
  k msg

exitVat :: Vat -> IO ()
exitVat = killThread . vatThreadId

------------------------------------------------------------------------

localActor :: RemoteRef -> Message -> Actor
localActor ref (Message "hello") = return (Now (Message "hi!"))
localActor ref (Message "hello2") = do
  a <- Free (RemoteCall ref (Message "hello") return)
  return (Now (Message "hi2!"))


remoteActor :: Message -> Actor
remoteActor (Message "hello") = return (Now (Message "oh hiiiiii!"))

test = do
  (channel1, channel2) <- stmChannel
  vat1 <- makeVat channel1
  vat2 <- makeVat channel2
  ref2 <- spawn vat2 remoteActor
  ref1 <- spawn vat1 (localActor (localToRemoteRef "localhost" ref2))
  let hello = Message "hello"
  resp1 <- call vat1 ref1 hello
  print resp1
  aresp2 <- send vat2 channel1 (localToRemoteRef "localhost" ref2)  hello
  resp2 <- wait aresp2
  print resp2
  resp3 <- call vat1 ref1 (Message "hello2")
  print resp3
 --  resp3 <- call vat1 ref1 hello
 --  print resp3
  -- on (send vat (RemoteRef "h" 0) (Message "foo1")) print
  -- on (send vat (RemoteRef "h" 0) (Message "foo2")) print
--  threadDelay 10000
--  stmRespond respVar (RequestId 1) (Message "bar2")
--  threadDelay 10000
--  stmRespond respVar (RequestId 0) (Message "bar1")
--  threadDelay 100000
--  exitVat vat
