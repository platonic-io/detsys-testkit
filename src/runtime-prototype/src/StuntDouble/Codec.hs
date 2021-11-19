{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StuntDouble.Codec where

import Data.Aeson (Value, Encoding, eitherDecode, encode, (.=))
import Data.Aeson.Encoding
       ( encodingToLazyByteString
       , pair
       , pairs
       , unsafeToEncoding
       )
import Data.Binary.Builder (fromLazyByteString)
import Data.ByteString.Lazy (ByteString)

import StuntDouble.Envelope
import StuntDouble.LogicalTime
import StuntDouble.Message
import StuntDouble.Reference

------------------------------------------------------------------------

data Codec = Codec
  { codecEncode :: Message -> ByteString
  , codecDecode :: ByteString -> Either String Message
  }

dummyCodec :: Codec
dummyCodec = Codec dummy dummy
  where
    dummy = error "dummyCodec: this should never be used."

encodeEnvelope :: Codec -> Envelope -> ByteString
encodeEnvelope (Codec enc _) e
  = encodingToLazyByteString
  $ envelopeToEncoding e { envelopeMessage = enc (envelopeMessage e) }

envelopeToEncoding :: Envelope' ByteString -> Encoding
envelopeToEncoding (Envelope
  (envelopeKind          :: EnvelopeKind)
  (envelopeSender        :: RemoteRef)
  (envelopeMessage       :: ByteString)
  (envelopeReceiver      :: RemoteRef)
  (envelopeCorrelationId :: CorrelationId)
  (envelopeLogicalTime   :: LogicalTimeInt))
  =
  pairs ("envelopeKind"          .= envelopeKind <>
         "envelopeSender"        .= envelopeSender <>
         (pair "envelopeMessage"  (unsafeToEncoding (fromLazyByteString envelopeMessage))) <>
         "envelopeReceiver"      .= envelopeReceiver <>
         "envelopeCorrelationId" .= envelopeCorrelationId <>
         "envelopeLogicalTime"   .= envelopeLogicalTime)

decodeEnvelope :: Codec -> ByteString -> Either String Envelope
decodeEnvelope (Codec _enc dec) bs =
  case eitherDecode bs of
    Left err -> Left err
    -- NOTE: Having to deal with `Value` here is a bit annoying, but there's
    -- probably no way around that unless we want to write our own JSON parser?
    Right (e' :: Envelope' Value) -> case dec (encode (envelopeMessage e')) of
      Left err  -> Left err
      Right msg -> Right (e' { envelopeMessage = msg })

-- XXX: would it make sense to group the envelope and message encoders like this?
--
-- data EnvelopeCodec enc = EnvelopeCodec
--   { encodeCodecEncodeEnvelope :: Envelope' enc -> ByteString
--   , encodeCodecEncodeMessage :: Message -> enc
--   }

jsonCodec :: Codec
jsonCodec = Codec encode eitherDecode

decodeJSONEnvelope :: ByteString -> Either String Envelope
decodeJSONEnvelope = decodeEnvelope jsonCodec

encodeJSONEnvelope :: Envelope -> ByteString
encodeJSONEnvelope = encodeEnvelope jsonCodec
