module StuntDouble.AdminTransport.NamedPipe where

import Control.Monad
import Text.Read (readMaybe)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error

import StuntDouble.Envelope
import StuntDouble.Message
import StuntDouble.Queue
import StuntDouble.Reference
import StuntDouble.AdminTransport
import StuntDouble.Transport.NamedPipe (safeCreateNamedPipe)

------------------------------------------------------------------------

namedPipeAdminTransport :: FilePath -> EventLoopName -> IO AdminTransport
namedPipeAdminTransport fp name = do
  queue <- newTBQueueIO 128 -- This queue grows if input is produced more often
                            -- than `transportReceive` is called.
  let pipe = fp </> getEventLoopName name <> "-admin"
  safeCreateNamedPipe pipe
  safeCreateNamedPipe (pipe <> "-response")
  h <- openFile pipe ReadWriteMode
  hSetBuffering h LineBuffering
  pid <- async (producer h queue)
  return AdminTransport
    { adminTransportSend = \s -> withFile (pipe <> "-response") ReadWriteMode $ \h' -> do
        hSetBuffering h' LineBuffering
        -- NOTE: We cannot write back the response on the same pipe as we got
        -- the command on, because `adminTransportRecieve` which runs in a loop
        -- will consume the response.
        hPutStrLn h' s
        putStrLn ("dumped log into " ++ pipe <> "-response")
    , adminTransportReceive  = atomically (flushTBQueue queue)
    , adminTransportShutdown = do
        adminCleanUpNamedPipe fp name
        cancel pid -- XXX: Potentially resource leak?
    }
  where
    producer :: Handle -> TBQueue AdminCommand -> IO ()
    producer h queue = forever $ do
      l <- hGetLine h
      case readMaybe l of
        Just cmd -> atomically (writeTBQueue queue cmd)
        Nothing  ->
          putStrLn ("namedPipeAdminTransport: unknown admin command: " ++ l)

adminCleanUpNamedPipe :: FilePath -> EventLoopName -> IO ()
adminCleanUpNamedPipe fp name =
  catchJust
    (\e -> if isDoesNotExistErrorType (ioeGetErrorType e)
           then Just ()
           else Nothing)
    (removeFile (fp </> getEventLoopName name <> "-admin") >>
     removeFile (fp </> getEventLoopName name <> "-admin-response"))
    return
