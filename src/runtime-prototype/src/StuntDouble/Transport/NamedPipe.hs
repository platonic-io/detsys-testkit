module StuntDouble.Transport.NamedPipe where

import Control.Exception
import Control.Concurrent.Async
import System.IO
import System.IO.Error
import System.Posix.Files
import System.FilePath

import StuntDouble.Reference
import StuntDouble.Message
import StuntDouble.Envelope
import StuntDouble.Transport

------------------------------------------------------------------------

namedPipeTransport :: FilePath -> EventLoopName -> IO (Transport IO)
namedPipeTransport fp name = do
  catchJust
    (\e -> if isAlreadyExistsErrorType (ioeGetErrorType e)
           then Just ()
           else Nothing)
    (createNamedPipe
      (fp </> getEventLoopName name)
      (namedPipeMode `unionFileModes`
       ownerReadMode `unionFileModes`
       ownerWriteMode))
    return
  h <- openFile (fp </> getEventLoopName name) ReadWriteMode
  hSetBuffering h LineBuffering
  return Transport { transportSend = \e ->
                       -- XXX:
                       withFile (fp </> address (envelopeReceiver e)) WriteMode $ \h' -> do
                         hSetBuffering h' LineBuffering
                         hPutStrLn h' (show e)
                   , transportReceive =
                       -- withFile (fp </> getEventLoopName name) ReadWriteMode $ \h -> do
                       --   hSetBuffering h LineBuffering
                         fmap read (hGetLine h)
                   }
