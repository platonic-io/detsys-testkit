module Lec03.QueueInterface where

data QueueI a = QueueI
  { qiEnqueue :: a -> IO Bool
  , qiDequeue :: IO (Maybe a)
  }
