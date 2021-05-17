module StuntDouble.Actor.State where

import Data.Aeson.Types
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

import StuntDouble.Datatype

------------------------------------------------------------------------

newtype State = State { getState :: HashMap Text Datatype }
  deriving Show

initState :: State
initState = State HashMap.empty

withHashMap :: (HashMap Text Datatype -> HashMap Text Datatype) -> State -> State
withHashMap f (State hm) = State (f hm)
withHashMap _f _otherwise = error "withHashMap: impossible, invalid state."

setField :: Text -> Datatype -> State -> State
setField k v = withHashMap (HashMap.insert k v)

getField :: Text -> State -> Datatype
getField k (State hm)  = hm HashMap.! k
getField _k _otherwise = error "getField: impossible, invalid state."

add :: Text -> Integer -> State -> State
add k v = withHashMap
  (\hm -> case HashMap.lookup k hm of
            Nothing          -> HashMap.insert k (Integer (fromInteger v)) hm
            Just (Integer i) -> HashMap.insert k (Integer (i + v)) hm)
