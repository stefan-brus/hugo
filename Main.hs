{-# OPTIONS_GHC -Wall #-}

-- Hugo main module
-- Connects to the irc server, joins a channel, and passes messages to hugo

module Main where

import Control.Applicative
import Control.Exception
import Control.Monad.State

import qualified Data.Map as M
import Data.List

import Network

import System.IO
import System.Time
import System.Random

import Text.Printf

import Config
import Hugo

-- Connect to server, run main loop, disconnect on exit
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st = do
      (v,_) <- runStateT run st
      return v

-- Connect to server and set up initial bot state
connect :: IO Bot
connect = notify $ do
  t <- getClockTime
  r <- getStdGen
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  return (Bot h t r M.empty False)
  where
    notify a = bracket_
      (printf "Connecting to %s ... " server >> hFlush stdout)
      (putStrLn "done.")
      a

-- Set up nick, join a channel, and start listening for commands
run :: Net ()
run = do
  write $ "NICK " ++ nick
  write $ "USER hugo 0 * :" ++ realname
  write $ "JOIN " ++ chan
  gets socket >>= listen

-- Process input from the IRC server, handle pinging and ponging
listen :: Handle -> Net ()
listen h = forever $ do
  s <- init <$> io (hGetLine h)
  io (putStrLn s)
  if ping s then pong s else eval (clean s)
  where
    clean = drop 1 . dropWhile (/=':') . drop 1
    ping x = "PING :" `isPrefixOf` x
    pong x = write $ "PONG " ++ (':' : drop 6 x)
