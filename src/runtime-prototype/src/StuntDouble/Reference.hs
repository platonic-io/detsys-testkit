{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module StuntDouble.Reference where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.String

------------------------------------------------------------------------

newtype LocalRef = LocalRef Int
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON LocalRef
instance ToJSON LocalRef

data RemoteRef = RemoteRef
  { address :: String
  , index :: Int
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON RemoteRef
instance ToJSON RemoteRef

localToRemoteRef :: EventLoopName -> LocalRef -> RemoteRef
localToRemoteRef name (LocalRef i) = RemoteRef (getEventLoopName name) i

remoteToLocalRef :: RemoteRef -> LocalRef
remoteToLocalRef = LocalRef . index

newtype EventLoopName = EventLoopName { getEventLoopName :: String }
  deriving (Eq, Ord, Show, IsString)

newtype ClientRef = ClientRef Int
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON ClientRef
instance FromJSON ClientRef
