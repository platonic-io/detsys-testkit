{-# language OverloadedStrings #-}
module StuntDouble.Transport.NamedPipeCodec where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Control.Concurrent.Async
import Control.Exception
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error
import System.Posix.Files
import System.Timeout

import StuntDouble.Codec
import StuntDouble.Envelope
import StuntDouble.Message
import StuntDouble.Reference
import StuntDouble.Transport

------------------------------------------------------------------------

namedPipeTransport :: FilePath -> EventLoopName -> Codec -> IO (Transport IO)
namedPipeTransport fp name codec@(Codec encode decode) = do
  safeCreateNamedPipe (fp </> getEventLoopName name)
  h <- openFile (fp </> getEventLoopName name) ReadWriteMode
  putStrLn $ "Listening on: " <> (fp </> getEventLoopName name)
  hSetBuffering h LineBuffering
  return Transport { transportSend = \e ->
                       let
                         addr    = address (envelopeReceiver e)
                         payload = encode (envelopeMessage e)
                       in
                         withFile (fp </> addr) WriteMode $ \h' -> do
                           hSetBuffering h' LineBuffering
                           BSL.hPutStr h' (payload <> "\n")
                   , transportReceive = do
                       m <- hMaybeGetLine h
                       case m of
                         Nothing -> return Nothing
                         Just resp -> do
                           putStrLn "Found input"
                           case decodeEnvelope codec resp of
                             Left err -> error ("transportReceive: couldn't parse response: " ++ show err)
                             Right envelope -> return . pure $ envelope
                   , transportShutdown = cleanUpNamedPipe fp name
                   }

safeCreateNamedPipe :: FilePath -> IO ()
safeCreateNamedPipe fp =
  catchJust
    (\e -> if isAlreadyExistsErrorType (ioeGetErrorType e)
           then Just ()
           else Nothing)
    (createNamedPipe fp
      (namedPipeMode `unionFileModes`
       ownerReadMode `unionFileModes`
       ownerWriteMode))
    return

cleanUpNamedPipe :: FilePath -> EventLoopName -> IO ()
cleanUpNamedPipe fp name =
  catchJust
    (\e -> if isDoesNotExistErrorType (ioeGetErrorType e)
           then Just ()
           else Nothing)
    (removeFile (fp </> getEventLoopName name))
    return

hMaybeGetLine :: Handle -> IO (Maybe BSL.ByteString)
hMaybeGetLine = timeout 10 . fmap BSLC.pack . hGetLine
