module Dumblog.ZeroCopy.State where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Internal as BS
import Data.Int (Int32, Int64)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as Vector
import Data.Word (Word16, Word64)
import Foreign (sizeOf)
import Network.Socket
       (Socket, SocketOption(Cork), setSocketOption)
import Network.Socket.ByteString (sendAll)
import Network.Socket.SendFile.Handle (sendFile')
import System.IO (Handle, IOMode(ReadMode), openFile)

import Journal.Types (hEADER_LENGTH)

------------------------------------------------------------------------

data State = State
  { sLocations     :: !(IOVector Location)
  , sNextIndex     :: !Int
  , sJournalHandle :: !Handle
  }

initState :: Int -> FilePath -> IO State
initState size fp
  = State
  <$> Vector.replicate size uninitialisedLocation
  <*> pure 0
  <*> openFile fp ReadMode

data Location = Location
  { lOffset :: !Word64
  , lLength :: !Word16
  }
  deriving Eq

uninitialisedLocation :: Location
uninitialisedLocation = Location 0 0

writeLocation :: State -> Int64 -> Location -> IO (Int, State)
writeLocation s offset loc = do
  let ix = sNextIndex s
      s' = s { sNextIndex = ix + 1 }
  Vector.write (sLocations s) ix
    (loc { lOffset = fromIntegral (offset - 4096) + lOffset loc })
  return (ix, s')

readLocation :: State -> Int -> IO (Maybe Location)
readLocation s ix = do
  loc <- Vector.read (sLocations s) ix
  if loc == uninitialisedLocation
  then return Nothing
  else return (Just loc)

readSendfile :: State -> Socket -> Int -> IO ()
readSendfile s sock ix = do
  mLoc <- readLocation s ix
  case mLoc of
    Nothing  -> sendAll sock notFound
    Just loc -> do
      -- setSocketOption sock Cork 1
      sendAll sock (httpHeader (lLength loc))
      -- hSeek (sJournalHandle s) AbsoluteSeek 0 -- (fromIntegral (lOffset loc))
      -- bs <- BS.create (fromIntegral (lLength loc + 100)) $ \ptr -> do
      --   _ <- hGetBuf (sJournalHandle s) ptr (fromIntegral (lLength loc + 100))
      --   return ()
      -- putStrLn ("readSendfile, hGetLine: `" ++ show bs ++ "'")
      _bytesSent <- sendFile' sock (sJournalHandle s)
                      (fromIntegral (lOffset loc) + fromIntegral hEADER_LENGTH +
                        (fromIntegral (sizeOf (4 :: Int32) + sizeOf (8 :: Int64))))
                      (fromIntegral (lLength loc))
      -- setSocketOption sock Cork 0
      return ()
  where
    notFound :: ByteString
    notFound = BS.pack "HTTP/1.0 404 Not Found\r\n\r\n"

    httpHeader :: Word16 -> ByteString
    httpHeader len =
      BS.pack "HTTP/1.0 200 OK\r\nContent-Length: " <> BS.pack (show len) <> BS.pack "\r\n\r\n"
