module StuntDouble.Message where

import StuntDouble.Reference

------------------------------------------------------------------------

data Message
  = InternalMessage String
  | ClientRequest String ClientRef
  deriving (Eq, Show, Read)

getMessage :: Message -> String
getMessage (InternalMessage msg) = msg
getMessage (ClientRequest msg _cref) = msg
