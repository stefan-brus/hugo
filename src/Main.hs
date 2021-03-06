-- Hugo main module
-- Connects to the irc server, joins a channel, and passes messages to hugo

module Main where

import Control.Applicative
import Control.Exception
import Control.Monad.State

import Data.List

import Network

import System.IO
import System.Time
import System.Random

import Text.Printf

import qualified Config
import Hugo

-- Connect to server, run main loop, disconnect on exit
main :: IO ()
main = bracket connect disconnect loop
  where
    loop st = do
      (v,_) <- runStateT run st
      return v

-- Connect to server and set up initial bot state
connect :: IO Bot
connect = notify $ do
  t <- getClockTime
  r <- getStdGen
  h <- connectTo Config.server (PortNumber (fromIntegral Config.port))
  pb <- readPhrasebook
  hSetBuffering h NoBuffering
  return (Bot h t r pb True True "")
  where
    notify a = bracket_
      (printf "Connecting to %s ... " Config.server >> hFlush stdout)
      (putStrLn "done.")
      a

-- Save the bot state to redis and close the socket
disconnect :: Bot -> IO ()
disconnect b = do
  hClose $ socket b

-- Set up nick, join a channel, and start listening for commands
run :: Net ()
run = do
  write $ "PASS " ++ Config.password
  write $ "NICK " ++ Config.nick
  write $ "USER hugo 0 * :" ++ Config.realname
  mapM_ (write . (++) "JOIN ") Config.chans
  gets socket >>= listen

-- Process input from the IRC server, handle pinging and ponging
listen :: Handle -> Net ()
listen h = forever $ do
  s <- init <$> io (hGetLine h)
  io (putStrLn s)
  if ping s then pong s else eval s
  where
    ping x = "PING :" `isPrefixOf` x
    pong x = write $ "PONG " ++ (':' : drop 6 x)
