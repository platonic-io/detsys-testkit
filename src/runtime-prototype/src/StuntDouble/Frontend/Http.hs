{-# LANGUAGE OverloadedStrings #-}

module StuntDouble.Frontend.Http where

import Control.Concurrent
import Control.Concurrent.Async
import Data.Aeson
import Data.String
import Network.HTTP.Client
import Network.HTTP.Types.Status
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp

import StuntDouble
import StuntDouble.ActorMap

------------------------------------------------------------------------

httpFrontend :: EventLoop -> LocalRef -> Wai.Application
httpFrontend ls lref req respond = do
  eMsg <- parseReq req
  case eMsg of
    Left err -> do
      respond (Wai.responseLBS status500 [] ("Couldn't parse request: " <> fromString err))
    Right msg -> do
      (_reply, aResp) <- clientRequest ls lref msg
      resp <- wait aResp
      respond (prettyResponse resp)

parseReq :: Wai.Request -> IO (Either String Message)
parseReq req = do
  body <- Wai.lazyRequestBody req
  return (eitherDecode body)

prettyResponse :: Message -> Wai.Response
prettyResponse msg = Wai.responseLBS status200 [] (encode msg)

withHttpFrontend :: EventLoop -> LocalRef -> Port -> (Async () -> IO a) -> IO a
withHttpFrontend ls lref port k =
  withAsync (run port (httpFrontend ls lref)) k

makeClientRequest :: Message -> Port -> IO (Either String Message)
makeClientRequest msg port = do
  manager <- newManager defaultManagerSettings
  let url :: String
      url = "http://localhost:" ++ show port

      body :: RequestBody
      body = RequestBodyLBS (encode msg)

  initialRequest <- parseRequest url

  let request =  initialRequest
                   { method      = "POST"
                   , requestBody = body
                   }

  respBody <- responseBody <$> httpLbs request manager
  return (eitherDecode respBody)
