module StuntDouble.Reference where

data LocalRef = LocalRef Int
  deriving (Eq, Ord)

data RemoteRef = RemoteRef
  { address :: String
  , index :: Int
  }
  deriving (Eq, Show, Read)

localToRemoteRef :: String -> LocalRef -> RemoteRef
localToRemoteRef address (LocalRef i) = RemoteRef address i

remoteToLocalRef :: RemoteRef -> LocalRef
remoteToLocalRef = LocalRef . index
