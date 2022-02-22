{-# LANGUAGE NumericUnderscores #-}

module Common where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS

------------------------------------------------------------------------

hOST :: String
hOST = "localhost"

pORT :: Int
pORT = 8054

wRITE_FREQUENCY :: Int
wRITE_FREQUENCY = 20

rEAD_FREQUENCY :: Int
rEAD_FREQUENCY = 80

nUM_OF_CLIENTS :: Int
nUM_OF_CLIENTS = 3

iTERATIONS :: Int
iTERATIONS = 10_000

vALUE_TO_WRITE :: ByteString
vALUE_TO_WRITE = LBS.pack "Dumblog"

bUFFER_CAPACITY :: Int
bUFFER_CAPACITY = 1024 * 64
