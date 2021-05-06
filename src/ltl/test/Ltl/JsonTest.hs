{-# LANGUAGE QuasiQuotes #-}

module Ltl.JsonTest where

import Data.Aeson.QQ.Simple
import Ltl.Json
import Test.HUnit

ex_target =
  [aesonQQ|{
     "title": "Goodbye!",
     "author" : {
       "givenName" : "John",
       "familyName" : "Doe"
     },
     "tags":[ "example", "sample" ],
     "content": "This will be unchanged"
   }|]

ex_patch =
  [aesonQQ| {
     "title": "Hello!",
     "phoneNumber": "+01-123-456-7890",
     "author": {
       "familyName": null
     },
     "tags": [ "example" ]
   }
|]

ex_result =
  [aesonQQ| {
     "title": "Hello!",
     "author" : {
       "givenName" : "John"
     },
     "tags": [ "example" ],
     "content": "This will be unchanged",
     "phoneNumber": "+01-123-456-7890"
   }
|]

unit_mergePatch :: Assertion
unit_mergePatch = do
  assertEqual "mergePatch matches example from rfc7396" ex_result (mergePatch ex_target ex_patch)
