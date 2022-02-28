module Dumblog.ZeroCopy.Main where

import Dumblog.ZeroCopy.HttpServer

------------------------------------------------------------------------

zeroCopyDumblog :: Int -> Int -> IO ()
zeroCopyDumblog _capacity port = httpServer port

main :: IO ()
main = zeroCopyDumblog (64 * 1024) 8054
