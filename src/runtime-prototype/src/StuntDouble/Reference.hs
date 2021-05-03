{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StuntDouble.Reference where

import Data.String

------------------------------------------------------------------------

data LocalRef = LocalRef Int
  deriving (Eq, Ord)

data RemoteRef = RemoteRef
  { address :: String
  , index :: Int
  }
  deriving (Eq, Show, Read)

localToRemoteRef :: EventLoopName -> LocalRef -> RemoteRef
localToRemoteRef name (LocalRef i) = RemoteRef (getEventLoopName name) i

remoteToLocalRef :: RemoteRef -> LocalRef
remoteToLocalRef = LocalRef . index

newtype EventLoopName = EventLoopName { getEventLoopName :: String }
  deriving (Eq, IsString)
