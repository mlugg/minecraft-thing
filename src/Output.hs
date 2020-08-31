{-# LANGUAGE LambdaCase #-}

module Output(sendPacket) where

import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder(Builder)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Network.Socket
import Network.Socket.ByteString

import Common

-- Simple types {{{

varInt :: Integer -> Builder
varInt x
  | x < bit 7 = B.word8 $ fromIntegral x
  | otherwise = B.word8 ((fromIntegral x .&. (bit 7 - 1)) .|. bit 7) <> varInt (x `shiftR` 7)

string :: Text -> Builder
string x = varInt (fromIntegral $ T.length x) <> B.byteString (E.encodeUtf8 x)

uByte :: Word8 -> Builder
uByte = B.word8

byte :: Int8 -> Builder
byte = uByte . fromIntegral

uShort :: Word16 -> Builder
uShort = B.word16BE

short :: Int16 -> Builder
short = uLong . fromIntegral

uInt :: Word32 -> Builder
uInt = B.word32BE

int :: Int32 -> Builder
int = uInt . fromIntegral

uLong :: Word64 -> Builder
uLong = B.word64BE

long :: Int64 -> Builder
long = uLong . fromIntegral

-- }}}

-- Packet transmission {{{

sendPacket :: Socket -> PacketOut -> IO ()
sendPacket s p =
  let (n,dat) = packet p
      raw = varInt n <> dat
      x' = B.toLazyByteString raw
      l = B.toLazyByteString
        $ varInt
        $ fromIntegral
        $ BSL.length x'
  in sendAll s $ BSL.toStrict $ l <> x'

-- }}}

packet = \case
  PResponse x -> (0x00, string x)
  PPong x     -> (0x01, long x)
