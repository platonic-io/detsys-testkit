module ATMC.Lec5.AwaitingClients where

import Control.Concurrent.MVar
import Data.ByteString.Lazy (ByteString)
import Data.IORef
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import ATMC.Lec5.StateMachine

------------------------------------------------------------------------

data AwaitingClients = AwaitingClients
  { acAwaitingClients :: IORef (IntMap (MVar ByteString))
  , acNextClientId    :: IORef Int
  }

newAwaitingClients :: IO AwaitingClients
newAwaitingClients = AwaitingClients <$> newIORef IntMap.empty <*> newIORef 0

addAwaitingClient :: AwaitingClients -> IO ClientId
addAwaitingClient ac = do
  i <- atomicModifyIORef' (acNextClientId ac) (\i -> (i + 1, i))
  resp <- newEmptyMVar
  atomicModifyIORef' (acAwaitingClients ac) (\im -> (IntMap.insert i resp im, ()))
  return (ClientId i)

removeAwaitingClient :: AwaitingClients -> ClientId -> IO ()
removeAwaitingClient ac (ClientId cid) =
  atomicModifyIORef' (acAwaitingClients ac) (\im -> (IntMap.delete cid im, ()))

respondToAwaitingClient :: AwaitingClients -> ClientId -> ByteString -> IO ()
respondToAwaitingClient ac (ClientId cid) resp = do
  im <- readIORef (acAwaitingClients ac)
  case IntMap.lookup cid im of
    Nothing -> return ()
    Just mvar -> putMVar mvar resp
