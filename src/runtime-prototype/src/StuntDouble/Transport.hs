module StuntDouble.Transport where

import StuntDouble.Envelope

------------------------------------------------------------------------

data TransportKind = NamedPipe FilePath | Http Int | Stm

data Transport m = Transport
  { transportSend    :: Envelope -> m ()
  , transportReceive :: m (Maybe Envelope)
  }
