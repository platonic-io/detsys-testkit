{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StuntDouble.Reference where

import Data.String

------------------------------------------------------------------------

newtype LocalRef = LocalRef Int
  deriving (Eq, Ord, Show)

data RemoteRef = RemoteRef
  { address :: String
  , index :: Int
  }
  deriving (Eq, Ord, Show, Read)

localToRemoteRef :: EventLoopName -> LocalRef -> RemoteRef
localToRemoteRef name (LocalRef i) = RemoteRef (getEventLoopName name) i

remoteToLocalRef :: RemoteRef -> LocalRef
remoteToLocalRef = LocalRef . index

newtype EventLoopName = EventLoopName { getEventLoopName :: String }
  deriving (Eq, Ord, Show, IsString)

newtype ClientRef = ClientRef Int
  deriving (Eq, Ord, Show)
