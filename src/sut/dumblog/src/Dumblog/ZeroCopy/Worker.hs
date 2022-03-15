{-# LANGUAGE OverloadedStrings #-}

module Dumblog.ZeroCopy.Worker where

import Control.Concurrent (threadDelay)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int32, Int64)
import Foreign (sizeOf)
import Foreign.C.Types (CInt(CInt))
import Network.Socket (mkSocket)
import Network.Socket.ByteString (sendAll)

import Journal.Internal.ByteBufferPtr
import Journal.Internal.Utils (int2Int32, int2Int64)
import Journal.MP
import Journal.Types (Journal, Subscriber(Sub1), hEADER_LENGTH)

import Dumblog.ZeroCopy.State

------------------------------------------------------------------------

worker :: Journal -> State -> IO ()
worker jour = go
  where
    go :: State -> IO ()
    go state = do
      mFdAndReq <- readJournal jour Sub1
      case mFdAndReq of
        Nothing -> threadDelay 10 >> go state
        Just fdAndReq -> do
          -- XXX: We should probably add a variant of readJournal that returns a
          -- bytebuffer instead of converting the bytestring to a bytebuffer...
          bb <- unsafeFromBS fdAndReq
          fd <- readInt32OffAddr bb 0
          offset <- readInt64OffAddr bb (sizeOf (4 :: Int32))
          let req = BS.drop (sizeOf (4 :: Int32) + sizeOf (8 :: Int64)) fdAndReq
          case parseCommand req of
            Just (Write offset' len) -> do
              (ix, state') <- writeLocation state offset
                                (Location (fromIntegral offset') (fromIntegral len))
              conn <- mkSocket (CInt fd)
              sendAll conn (response (BS.pack (show ix)))
              go state'
            Just (Read ix) -> do
              conn <- mkSocket (CInt fd)
              readSendfile state conn ix
              go state
            Nothing -> go state

response :: ByteString -> ByteString
response body =
  BS.pack "HTTP/1.0 200 OK\r\nContent-Length: " <> BS.pack (show (BS.length body)) <>
  "\r\n\r\n" <> body

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
