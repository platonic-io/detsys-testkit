module StuntDouble.EventLoop.RequestHandler where

import Control.Monad
import Control.Exception
import Control.Concurrent.Async
import Control.Concurrent.STM
import System.IO
import System.IO.Error
import System.Posix.Files

import StuntDouble.EventLoop.State
import StuntDouble.EventLoop.Event
import StuntDouble.EventLoop.Transport
import StuntDouble.Reference
import StuntDouble.Message

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
  return Transport { send    = hPutStrLn h . show
                   , receive = fmap read (hGetLine h)
                   }

------------------------------------------------------------------------

handleRequests :: LoopState -> IO ()
handleRequests ls = forever go
  where
    go = do
      e <- receive (loopStateTransport ls)
      atomically (writeTBQueue (loopStateQueue ls) (Receive (Request e)))

handleRequest :: Request -> LoopState -> IO ()
handleRequest (Request e) ls = undefined
  -- Treat this like a `Send (envelopeReceiver e) (envelopeMessage e)`?

------------------------------------------------------------------------

test :: IO ()
test = do
  t <- namedPipeTransport "/tmp/test_request.pipe"
  let e = Envelope (RemoteRef "from" 0) (Message "msg") (RemoteRef "to" 1)
  a <- async (send t e)
  e' <- receive t
  cancel a
  assert (e' == e) (return ())
