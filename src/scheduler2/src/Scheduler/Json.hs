module Scheduler.Json where

import qualified Data.Aeson as Aeson

type Json = Aeson.Value

null :: Json
null = Aeson.Null
