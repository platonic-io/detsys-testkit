module Codec where

import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS

data Envelope = Envelope
  { eLength :: Int
  , eContent :: ByteString -- maybe should be strict?
  }

encode :: Envelope -> ByteString
encode e = LBS.toStrict (Binary.encode (eLength e, eContent e))

-- this is guaranteed not to copy the bytestring
-- but we should probably allow this to fail
decode :: ByteString -> Envelope
decode bs =
  let (key, content) = Binary.decode $ LBS.fromStrict bs
  in Envelope key content
