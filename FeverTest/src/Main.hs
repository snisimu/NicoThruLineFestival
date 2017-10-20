{-# LANGUAGE OverloadedStrings #-} {- -*- Coding: utf-8 -*- -}

module Main where

import System.Environment
import System.Exit

import Control.Monad
import Control.Concurrent

import Data.Maybe
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Map.Strict as Map

import qualified Codec.Binary.UTF8.String as US

import Network.HTTP.Simple
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS

udpAddress = "239.0.0.24"
udpPort = "52460"

defaultText = "あいうえお"

type TheResponse = (Int, T.Text)

main :: IO ()
main = do
  args <- getArgs
  let
    (n,txt) = case args of
      m:[] -> (read m, defaultText) :: (Int, T.Text)
      m:t:_ -> (read m, T.pack t)
      _ -> (20, defaultText)
  sequence_ $ replicate n $ udpSend txt >> threadDelay (50 * 10^3)

udpSend :: T.Text -> IO ()
udpSend text = do
  let textBS8 = BS8.pack $ US.encodeString $ T.unpack text
  Sock.withSocketsDo $ do
    -- print textBS8 -- [Debug]
    info <- Sock.getAddrInfo (Just hint) (Just udpAddress) (Just udpPort)
    let saddr =  find (\i -> Sock.AF_INET == Sock.addrFamily i) info
    unless (isNothing saddr) $ do
      sock <- Sock.socket (Sock.addrFamily $ fromJust saddr) Sock.Datagram Sock.defaultProtocol
      SockBS.sendTo sock textBS8 (Sock.addrAddress $ fromJust saddr)
      Sock.close sock
      where
      hint = Sock.defaultHints { Sock.addrSocketType = Sock.Datagram, Sock.addrProtocol = 17 }
