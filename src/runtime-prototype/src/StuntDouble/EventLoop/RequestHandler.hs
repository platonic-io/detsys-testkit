module StuntDouble.EventLoop.RequestHandler where

import Control.Monad
import Control.Exception
import Control.Concurrent.Async
import Control.Concurrent.STM

import System.IO
import System.IO.Error
import System.Posix.Files

------------------------------------------------------------------------

-- XXX: dup
type Message = String
type RemoteRef = String
data LoopState = LoopState
  { loopStateQueue  :: TBQueue Event
  , loopStateAsyncs :: TVar [Async Message]
  , loopStateTransport :: Transport IO -- Will not change once created, so
                                       -- doesn't need STM?
  }
data Event = Receive Request
data Request = Request Envelope

------------------------------------------------------------------------

data Envelope = Envelope
  { envelopeSender   :: RemoteRef
  , envelopeMessage  :: Message
  , envelopeReceiver :: RemoteRef
  }
  deriving (Show, Read)

data Transport m = Transport
  { send    :: Envelope -> m ()
  , receive :: m Envelope
  }

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
  let msg = Envelope "from" "msg" "to"
  a <- async (send t msg)
  e <- receive t
  print e
  cancel a
