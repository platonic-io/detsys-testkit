module Assert (assert, assertM, assertMMsg, assertIO) where

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
    errMsg = unlines
      [ "Assertion failed"
      , prettyCallStack callStack
      ]

assertMMsg :: (HasCallStack, Monad m) => String -> Bool -> m ()
assertMMsg msg b = do
  if b
    then pure ()
    else throw (AssertionFailed errMsg)
  where
    errMsg = unlines
      [ "Assertion failed:"
      , msg
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
