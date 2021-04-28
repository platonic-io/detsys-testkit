module StuntDouble.EventLoop.Transport where

import StuntDouble.EventLoop.Event

------------------------------------------------------------------------

data Transport m = Transport
  { send    :: Envelope -> m ()
  , receive :: m Envelope
  }
