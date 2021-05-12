module StuntDouble.Message where

newtype Message = Message String
  deriving (Eq, Show, Read)

getMessage :: Message -> String
getMessage (Message msg) = msg
