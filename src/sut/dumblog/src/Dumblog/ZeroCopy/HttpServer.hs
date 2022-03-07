{-# LANGUAGE OverloadedStrings #-}

module Dumblog.ZeroCopy.HttpServer where

import Control.Concurrent
import Control.Exception (bracketOnError)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Int (Int64)
import GHC.Event
import Network.Socket
import Network.Socket.ByteString (sendAll)

import Journal.Internal.BufferClaim
import Journal.Internal.ByteBufferPtr
import Journal.MP
import Journal.Types (Journal, hEADER_LENGTH, jLogger)

import Dumblog.ZeroCopy.State

------------------------------------------------------------------------

httpServer :: Journal -> Int -> Maybe (MVar ()) -> IO ()
httpServer jour port mReady = withSocketsDo $ do
  numCapabilities <- getNumCapabilities
  putStrLn ("Starting http server on port: " ++ show port)
  putStrLn ("Capabilities: : " ++ show numCapabilities)
  state <- initState 40000 "/tmp/dumblog-zero-copy.journal"
  sock <- listenOn port
  mgr <- fromMaybe (error "Compile with -threaded") <$> getSystemEventManager
  _key <- withFdSocket sock $ \fd ->
    registerFd mgr (client mgr jour state sock) (fromIntegral fd) evtRead MultiShot
  threadDelay (1000 * 1000)
  maybe (return ()) (flip putMVar ()) mReady
  loop
  where
    loop = do
      threadDelay (10*1000*1000)
      loop

data Command = Write Int Int | Read Int
  deriving Show

parseCommand :: ByteString -> Maybe Command
parseCommand bs =
  let
    (method, rest) = BS.break (== ' ') bs
  in
    case method of
      "GET"  -> Read <$> parseIndex rest
      "POST" -> uncurry Write <$> parseOffsetLength rest
      _otherwise -> Nothing

parseIndex :: ByteString -> Maybe Int
parseIndex = fmap fst . BS.readInt . BS.dropWhile (\c -> c == ' ' || c == '/')

parseOffsetLength :: ByteString -> Maybe (Int, Int)
parseOffsetLength bs = do
  let (_before, match) = BS.breakSubstring "Content-Length: " bs
      rest             = BS.drop (BS.length "Content-Length: ") match
  (len, _rest) <- BS.readInt rest
  let (headers, _match) = BS.breakSubstring "\r\n\r\n" bs
  return (BS.length headers + BS.length "POST" + BS.length "\r\n\r\n", len)

bUFFER_SIZE :: Int
bUFFER_SIZE = 4096 - hEADER_LENGTH

client :: EventManager -> Journal -> State -> Socket -> FdKey -> Event -> IO ()
client mgr jour state sock fdKey event = do
  eRes <- tryClaim jour bUFFER_SIZE
  case eRes of
    Left err -> do
      putStrLn ("client, err: " ++ show err)
      client mgr jour state sock fdKey event
    Right (offset, bufferClaim) -> do
      (conn, _) <- accept sock
      -- setSocketOption conn NoDelay 1
      _key <- withFdSocket conn $ \fd ->
        registerFd mgr
          (client' mgr jour state conn offset bufferClaim) (fromIntegral fd) evtRead OneShot
      return ()

client' :: EventManager -> Journal -> State -> Socket -> Int64 -> BufferClaim -> FdKey
        -> Event -> IO ()
client' _mgr jour state conn offset bufferClaim _fdKey _event = do
  bytesRecv <- recvBytes bufferClaim conn bUFFER_SIZE
  -- XXX: commit after parsing?!
  commit bufferClaim (jLogger jour)
  req <- unsafeGetByteStringAt (bcByteBuffer bufferClaim) hEADER_LENGTH bytesRecv
  case parseCommand req of
    Just (Write offset' len) -> do
      ix <- writeLocation state offset (Location (fromIntegral offset') (fromIntegral len))
      sendAll conn (response (BS.pack (show ix)))
    Just (Read ix) -> readSendfile state conn ix
    Nothing -> return ()
  -- _key <- withFdSocket conn $ \fd ->
  --   registerFd mgr (client'' mgr jour state conn) (fromIntegral fd) evtRead OneShot
  -- return ()
  -- XXX: implement keep-alive...
  close conn

-- client'' :: EventManager -> Journal -> State -> Socket -> FdKey -> Event -> IO ()
-- client'' mgr jour state conn fdKey event = do
--   eRes <- tryClaim jour bUFFER_SIZE
--   case eRes of
--     Left err -> do
--       putStrLn ("client'', err: " ++ show err)
--       client'' mgr jour state conn fdKey event
--     Right (offset, bufferClaim) -> do
--       putStrLn "client'': before"
--       client' mgr jour state conn offset bufferClaim fdKey event
--       putStrLn "client'': after"

response :: ByteString -> ByteString
response body =
  BS.pack "HTTP/1.0 200 OK\r\nContent-Length: " <> BS.pack (show (BS.length body)) <>
  "\r\n\r\n" <> body

listenOn :: Int -> IO Socket
listenOn port = do
  addr <- resolve
  open addr
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) (Just "localhost") (Just (show port))

    open addr = bracketOnError (openSocket addr) (const (return ())) $ \sock -> do
      -- close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      -- setSocketOption sock NoDelay 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock (addrAddress addr)
      listen sock 1024
      return sock

openSocket :: AddrInfo -> IO Socket
openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
