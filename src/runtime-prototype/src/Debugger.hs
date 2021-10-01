module Debugger where

import Control.Concurrent.Async
import System.FilePath
import System.IO
import System.Posix.Files

import StuntDouble

------------------------------------------------------------------------

readLog :: IO Log
readLog = do
  let pipe = "/tmp" </> "scheduler-admin"
  -- NOTE: We need to start reading the response before making the request to
  -- dump the log, otherwise the response will be written to the void.
  a <- async (withFile (pipe <> "-response") ReadWriteMode hGetLine)
  appendFile pipe "AdminDumpLog\n"
  s <- wait a
  return (read s)

main :: IO ()
main = do
  l <- readLog
  print l
