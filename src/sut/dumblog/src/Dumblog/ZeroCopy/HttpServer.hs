{-# LANGUAGE OverloadedStrings #-}

module Dumblog.ZeroCopy.HttpServer where

import Control.Concurrent
import Control.Exception (bracketOnError)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Foreign (sizeOf)
import Foreign.C.Types (CInt(CInt))
import GHC.Event
import Network.Socket

import Journal.Internal.BufferClaim
import Journal.MP
import Journal.Types (Journal, hEADER_LENGTH, jLogger)

import Dumblog.ZeroCopy.State

------------------------------------------------------------------------

httpServer :: Journal -> Int -> Maybe (MVar ()) -> IO ()
httpServer jour port mReady = withSocketsDo $ do
  numCapabilities <- getNumCapabilities
  putStrLn ("Starting http server on port: " ++ show port)
  putStrLn ("Capabilities: : " ++ show numCapabilities)
  sock <- listenOn port
  mgr <- fromMaybe (error "Compile with -threaded") <$> getSystemEventManager
  _key <- withFdSocket sock $ \fd ->
    registerFd mgr (client mgr jour sock) (fromIntegral fd) evtRead MultiShot
  threadDelay (1000 * 1000)
  maybe (return ()) (flip putMVar ()) mReady
  loop
  where
    loop = do
      threadDelay (10*1000*1000)
      loop

bUFFER_SIZE :: Int
bUFFER_SIZE = 4096 - hEADER_LENGTH

client :: EventManager -> Journal -> Socket -> FdKey -> Event -> IO ()
client mgr jour sock fdKey event = do
  eRes <- tryClaim jour bUFFER_SIZE
  case eRes of
    Left err -> do
      putStrLn ("client, err: " ++ show err)
      client mgr jour sock fdKey event
    Right (offset, bufferClaim) -> do
      (conn, _) <- accept sock
      -- setSocketOption conn NoDelay 1
      _key <- withFdSocket conn $ \fd ->
        registerFd mgr
          (client' mgr jour conn offset bufferClaim) (fromIntegral fd) evtRead OneShot
      return ()

client' :: EventManager -> Journal -> Socket -> Int64 -> BufferClaim -> FdKey
        -> Event -> IO ()
client' _mgr jour conn offset bufferClaim _fdKey _event = do
  _bytesRecv <- recvBytesOffset
                  bufferClaim conn
                    (hEADER_LENGTH + sizeOf (4 :: Int32) + sizeOf (8 :: Int64)) bUFFER_SIZE
  CInt fd <- socketToFd conn
  putInt32At bufferClaim hEADER_LENGTH fd
  putInt64At bufferClaim (hEADER_LENGTH + sizeOf (4 :: Int32)) offset
  commit bufferClaim (jLogger jour)
  -- XXX: implement keep-alive...
  -- close conn

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
