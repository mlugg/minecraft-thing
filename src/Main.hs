{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import Network.Socket
import Network.Socket.ByteString
import Network.Run.TCP

import Common
import Parse
import Output

getPacket :: ConnState -> Socket -> (PacketIn -> IO ()) -> IO ()
getPacket cs s f = do
  dat <- recv s 2048
  let p = parsePacket cs dat
  case p of
    Left e -> putStrLn e -- TODO: good error handling
    Right x -> f x

handleConn :: ConnState -> Socket -> IO ()
handleConn cs s = getPacket cs s $ \case
  PHandshake vers addr port next -> do
    -- TODO: Should we check the received protocol version?
    -- TODO: We should definitely check the next state field, so that we
    -- don't do anything weird like going straight to Play
    peer <- getPeerName s
    putStrLn $ "Successful handshake with client at address " ++ show peer
    handleConn next s
  
  PRequest -> do
    sendPacket s (PResponse jsonResp)
    handleConn cs s

  PPing x -> do
    sendPacket s (PPong x)

jsonResp :: Text
jsonResp = "{ \"version\": { \"name\": \"1.16.2\", \"protocol\": 751 }, \"players\": { \"max\": 100, \"online\": 5, \"sample\": [ { \"name\": \"no really\", \"id\": \"4566e69f-c907-48ee-8d71-d7ba5aa00d20\" }, { \"name\": \"it actually works\", \"id\": \"4566e69f-c907-48ee-8d71-d7ba5aa00d20\" } ] }, \"description\": { \"text\": \"Hello, World!\" }}"
main :: IO ()
main = runTCPServer Nothing "25565" (handleConn Handshaking)
