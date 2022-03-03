{-# LANGUAGE OverloadedStrings #-}

module Dumblog.ZeroCopy.HttpServer where

import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Control.Exception (bracketOnError)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import GHC.Event
import Control.Concurrent

import Journal.Types (Journal, jLogger, hEADER_LENGTH)
import Journal.MP
import Journal.Internal.BufferClaim
import Journal.Internal.ByteBufferPtr

import Dumblog.ZeroCopy.State

------------------------------------------------------------------------

httpServer :: Journal -> Int -> IO ()
httpServer jour port = withSocketsDo $ do
  numCapabilities <- getNumCapabilities
  putStrLn ("Starting http server on port: " ++ show port)
  putStrLn ("Capabilities: : " ++ show numCapabilities)
  state <- initState 40000 "/tmp/dumblog-zero-copy.journal"
  sock <- listenOn port
  mgr <- fromMaybe (error "Compile with -threaded") <$> getSystemEventManager
  _key <- withFdSocket sock $ \fd ->
    registerFd mgr (client jour state sock) (fromIntegral fd) evtRead MultiShot
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

client :: Journal -> State -> Socket -> FdKey -> Event -> IO ()
client jour state sock fdKey event = do
  let bufSize = 4096 - hEADER_LENGTH
  eRes <- tryClaim jour bufSize
  case eRes of
    Left err -> do
      putStrLn ("client, err: " ++ show err)
      client jour state sock fdKey event
    Right (offset, bufferClaim) -> do
      (conn, _) <- accept sock
      bytesRecv <- recvBytes bufferClaim conn bufSize
      commit bufferClaim (jLogger jour)

      -- NOTE: The following copies bytes. It would be better to do the parsing at
      -- the `ByteBuffer` level rather than `ByteString` level...
      req <- getByteStringAt (bcByteBuffer bufferClaim) hEADER_LENGTH bytesRecv
      case parseCommand req of
        Just (Write offset' len) -> do
          ix <- writeLocation state offset (Location (fromIntegral offset') (fromIntegral len))
          sendAll conn (response (BS.pack (show ix)))
        Just (Read ix) -> readSendfile state conn ix
        Nothing -> return ()
      close conn

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
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock (addrAddress addr)
      listen sock 1024
      return sock

openSocket :: AddrInfo -> IO Socket
openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
