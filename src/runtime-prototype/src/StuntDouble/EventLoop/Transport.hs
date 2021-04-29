module StuntDouble.EventLoop.Transport where

import Control.Exception
import Control.Concurrent.Async
import System.IO
import System.IO.Error
import System.Posix.Files

import StuntDouble.EventLoop.Event
import StuntDouble.Reference
import StuntDouble.Message

------------------------------------------------------------------------

data Transport m = Transport
  { transportSend    :: Envelope -> m ()
  , transportReceive :: m Envelope
  }

------------------------------------------------------------------------

namedPipeTransport :: FilePath -> IO (Transport IO)
namedPipeTransport fp = do
  catchJust
    (\e -> if isAlreadyExistsErrorType (ioeGetErrorType e)
           then Just ()
           else Nothing)
    (createNamedPipe fp (namedPipeMode `unionFileModes`
                         ownerReadMode `unionFileModes`
                         ownerWriteMode))
    return
  h <- openFile fp ReadWriteMode
  hSetBuffering h LineBuffering
  return Transport { transportSend    = hPutStrLn h . show
                   , transportReceive = fmap read (hGetLine h)
                   }

------------------------------------------------------------------------

test :: IO ()
test = do
  t <- namedPipeTransport "/tmp/test_request.pipe"
  let e = Envelope (RemoteRef "from" 0) (Message "msg") (RemoteRef "to" 1) 0
  a <- async (transportSend t e)
  e' <- transportReceive t
  cancel a
  assert (e' == e) (return ())
