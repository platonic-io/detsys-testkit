module FrontEnd where

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS

import Network.HTTP.Types.Status (status200, status400)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp

import Journal (Journal)
import qualified Journal
import Journal.Types.AtomicCounter (AtomicCounter)
import qualified Journal.Types.AtomicCounter as AtomicCounter

import Blocker
import Types

data FrontEndInfo = FrontEndInfo
  { sequenceNumber :: AtomicCounter
  , blockers :: Blocker (Either Response Response)
  }

httpFrontend :: Journal -> FrontEndInfo -> Wai.Application
httpFrontend journal (FrontEndInfo c blocker) req respond = do
  body <- Wai.strictRequestBody req
  key <- AtomicCounter.incrCounter 1 c
  Journal.appendBS journal (LBS.toStrict $ Binary.encode (key, body))
  resp <- blockUntil blocker key
  Journal.dumpJournal journal
  case resp of
    Left errMsg -> respond $ Wai.responseLBS status400 [] errMsg
    Right msg -> respond $ Wai.responseLBS status200 [] msg

runFrontEnd :: Int -> Journal -> FrontEndInfo -> IO ()
runFrontEnd port journal feInfo = run port (httpFrontend journal feInfo)
