module Journal.Internal where

import Control.Concurrent.Async

import Journal.Types

------------------------------------------------------------------------

claim :: Int -> Journal -> IO Position
claim bytes = undefined
  -- if bytes + offset <= jMaxSize then write to active file
  -- if bytes + offset > jMaxSize then
  --    if offset - jMaxSize == bytes then rotate files
  --    e.g. max size = 1000, we are trying to write 100 bytes and the offset we get is 1100

  --    if offset - jMaxSize > bytes then somebody else is responsive for
  --    rotating, we know that's done when jSequence is bumped?

  --    continuing on the above example say we are trying to write 100 bytes we get offset 1200
  -- if bytes + offset > jMaxS

-- | "active" file becomes "dirty", and the "clean" file becomes the new
-- "active" file.
rotateFiles :: Journal -> IO ()
rotateFiles = undefined

-- Assumption: cleaning the dirty file takes shorter amount of time than filling
-- up the active file to its max size.
cleanDirtyFile :: Journal -> IO ()
cleanDirtyFile = undefined

spawnCleaningThread :: IO (Async ())
spawnCleaningThread = undefined

data Inconsistency = Inconsistency

checkForInconsistencies :: Journal -> IO [Inconsistency]
checkForInconsistencies = undefined

fixInconsistency :: Inconsistency -> Journal -> IO ()
fixInconsistency = undefined
