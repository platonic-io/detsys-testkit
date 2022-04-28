module ATMC.Lec3.Service where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.ByteString.Lazy (ByteString)
import Data.IORef
import qualified Data.ByteString.Lazy.Char8 as BS8
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment

import ATMC.Lec3.Queue
import ATMC.Lec3.QueueTest (fakeEnqueue, fakeDequeue, newModel)
import ATMC.Lec3.QueueInterface

------------------------------------------------------------------------

mAX_QUEUE_SIZE :: Int
mAX_QUEUE_SIZE = 128

pORT :: Int
pORT = 8050

------------------------------------------------------------------------

realQueue :: Int -> IO (QueueI a)
realQueue size = do
  q <- newQueue size
  return QueueI
    { qiEnqueue = enqueue q
    , qiDequeue = dequeue q
    }

fakeQueue :: Int -> IO (QueueI a)
fakeQueue size = do
  ref <- newIORef (newModel size)
  return QueueI
    { qiEnqueue = \x -> atomicModifyIORef' ref (fakeEnqueue x)
    , qiDequeue =       atomicModifyIORef' ref fakeDequeue
    }

------------------------------------------------------------------------

main :: IO ()
main = do
  args  <- getArgs
  queue <- case args of
             ["--testing"] -> fakeQueue mAX_QUEUE_SIZE
             _otherwise    -> realQueue mAX_QUEUE_SIZE
  service queue

service :: QueueI Command -> IO ()
service queue = do
  withAsync (worker queue) $ \_a ->
    runFrontEnd queue pORT

worker :: QueueI Command -> IO ()
worker queue = go initState
  where
    go :: State -> IO ()
    go state = do
      cmd <- undefined -- atomically (readTBQueue queue)
      state' <- execute cmd state
      go state'

data Command
  = Write ByteString (MVar Int)
  | Read Int (MVar (Maybe ByteString))

data State = State

initState :: State
initState = State

execute :: Command -> State -> IO State
execute (Read ix response) state = do
  bs <- undefined -- readDB conn ix
  putMVar response bs
  undefined
execute (Write bs response) state = do
  ix <- undefined -- writeDB conn bs
  putMVar response ix
  undefined

httpFrontend :: QueueI Command -> Application
httpFrontend queue req respond =
  case requestMethod req of
    "GET" -> do
      case parseIndex of
        Left err -> do
          respond (responseLBS status400 [] err)
        Right ix -> do
          response <- newEmptyMVar
          -- atomically (writeTBQueue queue (Read ix response))
          mbs <- takeMVar response
          case mbs of
            Nothing ->
              respond (responseLBS status404 [] (BS8.pack "Not found"))
            Just bs -> respond (responseLBS status200 [] bs)
    "POST" -> do
      bs <- consumeRequestBodyStrict req
      response <- newEmptyMVar
      -- atomically (writeTBQueue queue (Write bs response))
      ix <- takeMVar response :: IO Int
      respond (responseLBS status200 [] (BS8.pack (show ix)))
    _otherwise -> do
      respond (responseLBS status400 [] "Invalid method")
  where
    parseIndex :: Either ByteString Int
    parseIndex = undefined
  {-
      case pathInfo req of
        [txt] -> case decimal txt of
          Right (ix, _rest) -> Right ix
          _otherwise -> Left (BS8.pack "parseIndex: GET /:ix, :ix isn't an integer")
        _otherwise   -> Left (BS8.pack "parseIndex: GET /:ix, :ix missing")
-}

runFrontEnd :: QueueI Command -> Port -> IO ()
runFrontEnd queue port = run port (httpFrontend queue)
