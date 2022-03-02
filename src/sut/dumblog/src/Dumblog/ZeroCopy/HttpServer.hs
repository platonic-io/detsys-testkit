{-# LANGUAGE OverloadedStrings #-}

module Dumblog.ZeroCopy.HttpServer where

import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Control.Exception (bracketOnError)
import Network.Socket
import Network.Socket.ByteString (sendAll, recv)
import GHC.Event
import Control.Concurrent

import Journal.Types (Journal, jLogger, hEADER_LENGTH)
import Journal.MP
import Journal.Internal.BufferClaim
import Journal.Internal.ByteBufferPtr

------------------------------------------------------------------------

httpServer :: Journal -> Int -> IO ()
httpServer jour port = withSocketsDo $ do
  numCapabilities <- getNumCapabilities
  putStrLn ("Starting http server on port: " ++ show port)
  putStrLn ("Capabilities: : " ++ show numCapabilities)
  sock <- listenOn port
  mgr <- fromMaybe (error "Compile with -threaded") <$> getSystemEventManager
  _key <- withFdSocket sock $ \fd ->
    registerFd mgr (client jour sock) (fromIntegral fd) evtRead MultiShot
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

client :: Journal -> Socket -> FdKey -> Event -> IO ()
client jour sock fdKey event = do
  eRes <- tryClaim jour 4096
  case eRes of
    Left err -> do
      putStrLn ("client, err: " ++ show err)
      client jour sock fdKey event
    Right (offset, bufferClaim) -> do
      (conn, _) <- accept sock
      bytesRecv <- recvBytes bufferClaim conn 4096
      putStrLn ("client, bytes received: " ++ show bytesRecv)
      commit bufferClaim (jLogger jour)

      -- NOTE: The following copies bytes. It would be better to do the parsing at
      -- the `ByteBuffer` level rather than `ByteString` level...
      req <- getByteStringAt (bcByteBuffer bufferClaim) hEADER_LENGTH bytesRecv

      print req
      case parseCommand req of
        Just (Write offset len) ->
          putStrLn ("BODY: " ++ BS.unpack (BS.take len (BS.drop offset req)))
        Just (Read _ix) -> return ()
        Nothing -> return ()
      sendAll conn msg
      close conn

msg :: ByteString
msg = BS.pack "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"


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

-- foreign import ccall unsafe "sys/sendfile.h sendfile"
--   c_sendfile :: Fd -> Fd -> Ptr Int64 -> Word64 -> IO Int64
