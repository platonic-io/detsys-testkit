module Assert (assert, assertM, assertIO) where

import Control.Exception (throw, AssertionFailed(AssertionFailed))
import GHC.Stack (HasCallStack, callStack, prettyCallStack)

assert :: HasCallStack => Bool -> a -> a
assert True x = x
assert False _ = throw (AssertionFailed errMsg)
  where
    errMsg = concat
      [ "Assertion failed\n"
      , prettyCallStack callStack
      ]

assertM :: (HasCallStack, Monad m) => Bool -> m ()
assertM b = do
  if b
    then pure ()
    else throw (AssertionFailed errMsg)
  where
    errMsg = concat
      [ "Assertion failed\n"
      , prettyCallStack callStack
      ]

assertIO :: HasCallStack => IO Bool -> IO ()
assertIO mb = do
  b <- mb
  if b
    then pure ()
    else throw (AssertionFailed errMsg)
  where
    errMsg = concat
      [ "Assertion failed\n"
      , prettyCallStack callStack
      ]
