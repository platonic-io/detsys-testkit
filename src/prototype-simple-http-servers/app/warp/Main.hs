module Main where

import qualified Data.ByteString.Lazy.Char8 as BSChar8
import Network.HTTP.Types.Status (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (Port, run)

------------------------------------------------------------------------

app :: Application
app _req respond =
  respond (responseLBS status200 [] (BSChar8.pack "Pong!"))

main :: IO ()
main = do
  putStrLn "Starting warp http server on port 5002"
  run 5002 app
