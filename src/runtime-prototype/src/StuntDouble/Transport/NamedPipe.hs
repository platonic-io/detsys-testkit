module StuntDouble.Transport.NamedPipe where

import Control.Concurrent.Async
import Control.Exception
import System.FilePath
import System.IO
import System.IO.Error
import System.Posix.Files
import System.Timeout

import StuntDouble.Envelope
import StuntDouble.Message
import StuntDouble.Reference
import StuntDouble.Transport

------------------------------------------------------------------------

namedPipeTransport :: FilePath -> EventLoopName -> IO (Transport IO)
namedPipeTransport fp name = do
  safeCreateNamedPipe (fp </> getEventLoopName name)
  h <- openFile (fp </> getEventLoopName name) ReadWriteMode
  hSetBuffering h LineBuffering
  return Transport { transportSend = \e ->
                       withFile (fp </> address (envelopeReceiver e)) WriteMode $ \h' -> do
                         hSetBuffering h' LineBuffering
                         hPutStrLn h' (show e)
                   , transportReceive =
                       fmap (fmap read) (hMaybeGetLine h)
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

hMaybeGetLine :: Handle -> IO (Maybe String)
hMaybeGetLine = timeout 1000 . hGetLine
