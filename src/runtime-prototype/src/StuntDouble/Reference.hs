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

localToRemoteRef :: String -> LocalRef -> RemoteRef
localToRemoteRef address (LocalRef i) = RemoteRef address i

remoteToLocalRef :: RemoteRef -> LocalRef
remoteToLocalRef = LocalRef . index

newtype EventLoopName = EventLoopName { getEventLoopName :: String }
  deriving IsString
