module StuntDouble.Actor.State where

import Data.Aeson.Types
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

import StuntDouble.Datatype

------------------------------------------------------------------------

newtype State = State { getHashMap :: HashMap Text Datatype }
  deriving Show

stateFromList :: [(Text, Datatype)] -> State
stateFromList = State . HashMap.fromList

emptyState :: State
emptyState = State HashMap.empty

withHashMap :: (HashMap Text Datatype -> HashMap Text Datatype) -> State -> State
withHashMap f (State hm) = State (f hm)
withHashMap _f _otherwise = error "withHashMap: impossible, invalid state."

setField :: Text -> Datatype -> State -> State
setField k v = withHashMap (HashMap.insert k v)

getField :: Text -> State -> Datatype
getField k (State hm)  = hm HashMap.! k
getField _k _otherwise = error "getField: impossible, invalid state."

modifyField :: Text -> (Datatype -> Datatype) -> State -> State
modifyField k f = withHashMap (HashMap.adjust f k)

add :: Text -> Integer -> State -> State
add k v = withHashMap (HashMap.alter (Just . maybe (Integer v) (plus (Integer v))) k)
