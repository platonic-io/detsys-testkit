module StuntDouble.AdminTransport where

------------------------------------------------------------------------

data AdminTransportKind = AdminNamedPipe FilePath

data AdminCommand
  = AdminQuit
  | AdminDumpLog
  | AdminResetLog
  deriving (Read, Show)

data AdminTransport = AdminTransport
  { adminTransportSend     :: String -> IO ()
  , adminTransportReceive  :: IO [AdminCommand]
  , adminTransportShutdown :: IO ()
  }
