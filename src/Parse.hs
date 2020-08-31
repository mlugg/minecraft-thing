{-# LANGUAGE LambdaCase #-}

module Parse(parsePacket) where

import Data.Attoparsec.ByteString(Parser)
import qualified Data.Attoparsec.ByteString as P

import qualified Data.Text.Encoding as E

import Common

-- Simple types {{{

wordBE :: (Integral a, Bits a) => Int -> Parser a
wordBE n = do
  xs <- P.count n P.anyWord8
  pure $ foldl f 0 xs
  where f s x = s `shiftL` 8 .|. fromIntegral x

bool :: Parser Bool
bool = do
  x <- P.anyWord8
  pure $ x /= 0

uByte :: Parser Word8
uByte = P.anyWord8

byte :: Parser Int8
byte = fromIntegral <$> uByte

uShort :: Parser Word16
uShort = wordBE 2

short :: Parser Int16
short = fromIntegral <$> uShort

uInt :: Parser Word32
uInt = wordBE 4

int :: Parser Int32
int = fromIntegral <$> uInt

uLong :: Parser Word64
uLong = wordBE 8

long :: Parser Int64
long = fromIntegral <$> uLong

varInt' :: Parser (Int, Int)
varInt' = do
  x <- fromIntegral <$> P.anyWord8
  let x' = x `clearBit` 7
  if x `testBit` 7
  then do
    (y, n) <- varInt'
    pure (x' .|. y `shiftL` 7, n+1)
  else pure (x', 1)

varInt :: Parser Int
varInt = fst <$> varInt'

string :: Parser Text
string = do
  n <- varInt
  x <- P.take n
  case E.decodeUtf8' x of
    Left e -> fail $ show e
    Right x -> pure x

-- }}}

-- Packet parser {{{

packet :: ConnState -> Parser PacketIn
packet s = do
  len <- varInt
  (packId,n) <- varInt'
  let bodyLen = len - n
  body <- P.take bodyLen
  P.endOfInput
  case P.parseOnly (packet' s packId <* P.endOfInput) body of
    Left e -> fail $ "Error with packet ID " <> show packId
    Right x -> pure x

parsePacket :: ConnState -> ByteString -> Either String PacketIn
parsePacket = P.parseOnly . packet

-- }}}

packet' Handshaking = \case
  0 -> PHandshake <$> varInt <*> string <*> uShort <*> (toEnum <$> varInt)
  x -> fail $ "Unknown packet ID in Handshaking state: " <> show x
      
packet' Status = \case
  0 -> pure PRequest
  1 -> PPing <$> long
  x -> fail $ "Unknown packet ID in Status state: " <> show x
