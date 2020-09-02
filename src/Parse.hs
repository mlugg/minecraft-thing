{-# LANGUAGE LambdaCase #-}

module Parse(parsePacket) where

import Text.Megaparsec
import Control.Monad.Combinators
import Data.Void

import qualified Data.Text.Encoding as E

import Common

type Parser = Parsec Void ByteString

-- Simple types {{{

anyWord8 = anySingle

wordBE :: (Integral a, Bits a) => Int -> Parser a
wordBE n = do
  xs <- count n anyWord8
  pure $ foldl f 0 xs
  where f s x = s `shiftL` 8 .|. fromIntegral x

bool :: Parser Bool
bool = do
  x <- anyWord8
  pure $ x /= 0

uByte :: Parser Word8
uByte = anyWord8

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
  x <- fromIntegral <$> anyWord8
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
  x <- takeP Nothing n
  case E.decodeUtf8' x of
    Left e -> fail $ show e
    Right x -> pure x

-- }}}

-- Packet parser {{{

packet :: ConnState -> Parser PacketIn
packet s = do
  len <- varInt
  (packId,n) <- varInt'
  let bodyLen = len - n -- currently unused
  packet' s packId <* eof

parsePacket :: ConnState -> ByteString -> Either (ParseErrorBundle ByteString Void) PacketIn
parsePacket cs = parse (packet cs) "<network packet>"

-- }}}

packet' Handshaking = \case
  0 -> PHandshake <$> varInt <*> string <*> uShort <*> (toEnum <$> varInt)
  x -> fail $ "Unknown packet ID in Handshaking state: " <> show x
      
packet' Status = \case
  0 -> pure PRequest
  1 -> PPing <$> long
  x -> fail $ "Unknown packet ID in Status state: " <> show x
