{-# LANGUAGE Safe #-}

module Common
  ( module Common
  , module Data.Text
  , module Data.ByteString
  , module Data.Word
  , module Data.Int
  , module Data.Bits
  , module Control.Applicative
  , module Control.Monad ) where

import Data.Text(Text)
import Data.ByteString(ByteString)
import Data.Word
import Data.Int
import Data.Bits
import Control.Applicative
import Control.Monad

data ConnState
  = Handshaking
  | Status
  | Login
  | Play
  deriving (Show, Enum)

data PacketIn
  = PHandshake Int Text Word16 ConnState
  | PRequest
  | PPing Int64
  deriving (Show)

data PacketOut
  = PResponse Text
  | PPong Int64
  deriving (Show)
