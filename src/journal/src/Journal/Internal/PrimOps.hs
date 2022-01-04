{-# LANGUAGE ForeignFunctionInterface #-}

module Journal.Internal.PrimOps where

import Foreign

------------------------------------------------------------------------

foreign import ccall unsafe "stdatomic.h __atomic_fetch_add_8"
  c_fetch_and_add_word :: Ptr Word -> Word -> IO Word

cFetchAndAddWord :: Ptr Word -> Word -> IO Word
cFetchAndAddWord = c_fetch_and_add_word

main :: IO ()
main = do
  ptr <- callocBytes 1
  zero <- cFetchAndAddWord ptr 2
  print zero
  two <- peekByteOff ptr 0 :: IO Word
  print two
