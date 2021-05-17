module StuntDouble.Actor.State where

import Data.Aeson.Types
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

------------------------------------------------------------------------

newtype State = State { getState :: Value }
  deriving Show

initState :: State
initState = State emptyObject

withHashMap :: (HashMap Text Value -> HashMap Text Value) -> State -> State
withHashMap f (State (Object hm)) = State (Object (f hm))
withHashMap _f _otherwise = error "withHashMap: impossible, invalid state."

setField :: Text -> Value -> State -> State
setField k v = withHashMap (HashMap.insert k v)

getField :: Text -> State -> Value
getField k (State (Object hm)) = hm HashMap.! k
getField _k _otherwise = error "getField: impossible, invalid state."

add :: Text -> Integer -> State -> State
add k v = withHashMap
  (\hm -> case HashMap.lookup k hm of
            Nothing         -> HashMap.insert k (Number (fromInteger v)) hm
            Just (Number i) -> HashMap.insert k (Number (i + fromInteger v)) hm)
