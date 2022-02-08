{-# LANGUAGE TypeFamilies #-}
module Journal.Internal.Logger where

import Data.Text (Text, pack)
import qualified Data.Text.IO as TextIO
import GHC.IO.Handle (Handle)

newtype Logger = Logger
  { logText :: Text -> IO ()
  }

class CanLog s where
  logg :: Logger -> s -> IO ()

instance CanLog Text where
  logg = logText

instance (a~Char) => CanLog [a] where
  logg logger str = logText logger (pack str)

handleLogger :: Handle -> Logger
handleLogger h = Logger (TextIO.hPutStrLn h)

ioLogger :: Logger
ioLogger = Logger (TextIO.putStrLn)

nullLogger :: Logger
nullLogger = Logger (const $ pure ())
