module StuntDouble.EventLoop.Transport where

import Control.Exception
import Control.Concurrent.Async
import System.IO
import System.IO.Error
import System.Posix.Files
import System.FilePath

import StuntDouble.EventLoop.Event
import StuntDouble.Reference
import StuntDouble.Message

------------------------------------------------------------------------

data Transport m = Transport
  { transportSend    :: Envelope -> m ()
  , transportReceive :: m Envelope
  }

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
                   , transportReceive = fmap read (hGetLine h)
                   }

------------------------------------------------------------------------

test :: IO ()
test = do
  t <- namedPipeTransport "/tmp" (EventLoopName "a")
  let e = Envelope (RemoteRef "from" 0) (Message "msg") (RemoteRef "a" 1) 0
  a <- async (transportSend t e)
  e' <- transportReceive t
  cancel a
  assert (e' == e) (return ())
