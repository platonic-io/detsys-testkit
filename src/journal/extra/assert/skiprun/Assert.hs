module Assert (assert, assertM, assertIO) where

import GHC.Stack (HasCallStack)

{-# INLINE assert #-}
assert :: Bool -> a -> a
assert _ = id

{-# INLINE assertM #-}
assertM :: (Monad m) => Bool -> m ()
assertM _ = pure ()

{-# INLINE assertIO #-}
assertIO :: IO Bool -> IO ()
assertIO _ = pure ()
