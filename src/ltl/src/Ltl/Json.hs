module Ltl.Json where

import qualified Data.Aeson as Aeson
import Data.Aeson.QQ.Simple
import Data.List (sort)
import qualified Data.Maybe as Maybe
import Data.Text(Text)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as TextEncoding
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector

type Json = Aeson.Value

data JQ
  = This
  | Lookup JQ Text
  | Index JQ Integer
  deriving (Eq, Show)

jq :: JQ -> Json -> Json
jq This js = js
jq (Lookup f k) js = case js of
  Aeson.Object hm -> case HashMap.lookup k hm of
    Nothing -> error $ "Unknown key " ++ show k-- Aeson.Null
    Just js' -> jq f js'
  _ -> Aeson.Null -- or fail?
jq (Index f i) js = case js of
  Aeson.Array xs ->case xs Vector.!? (fromInteger i) of
    Just js' -> jq f js'
    Nothing -> Aeson.Null
  _ -> Aeson.Null

--------------------------------------------------------------------------------

decode :: Text -> Either String Json
decode = Aeson.eitherDecode . TextEncoding.encodeUtf8 . LazyText.fromStrict
--------------------------------------------------------------------------------

{-
   https://tools.ietf.org/html/rfc7396

   define MergePatch(Target, Patch):
     if Patch is an Object:
       if Target is not an Object:
         Target = {} # Ignore the contents and set it to an empty Object
       for each Name/Value pair in Patch:
         if Value is null:
           if Name exists in Target:
             remove the Name/Value pair from Target
         else:
           Target[Name] = MergePatch(Target[Name], Value)
       return Target
     else:
       return Patch
-}

mergePatch :: Json -> Json -> Json
mergePatch target (Aeson.Object hm) =
  let
    targetKV = case target of
        Aeson.Object kv -> kv
        _ -> HashMap.empty
    go tKv [] = Aeson.Object tKv
    go tKv ((k, v):ps) = go (HashMap.alter f k tKv) ps
      where
        f tv = case v of
          Aeson.Null -> Nothing
          _ -> Just $ mergePatch (Maybe.fromMaybe Aeson.Null tv) v
  in go targetKV (sort $ HashMap.toList hm) -- sort to make deterministic?
mergePatch _ patch = patch
