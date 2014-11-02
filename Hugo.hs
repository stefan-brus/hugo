{-# OPTIONS_GHC -Wall #-}

-- Hugo bot module
-- Contains the logic for handling various commands

module Hugo where

import Control.Monad.Reader

import Data.List

import System.Exit
import System.IO

import Text.Printf

import Config

-----------
-- TYPES --
-----------

data Bot = Bot { socket :: Handle }

type Net = ReaderT Bot IO

data Command =
    Quit
  | Id

--------------------------
-- HUGO LOGIC FUNCTIONS --
--------------------------

-- Check what the given command is and evaluate it
eval :: String -> Net ()
eval x = case command x of
  Just Quit -> quit
  Just Id -> say x
  Nothing -> return ()

-- Quit!
quit :: Net ()
quit = write "QUIT :Exiting" >> io (exitWith ExitSuccess)

-- Say the given string
say :: String -> Net ()
say = privmsg . drop 4

-- See what command the given string is, or nothing if it isn't one
command :: String -> Maybe Command
command x
  | is "quit" x = Just Quit
  | is "id" x = Just Id
  | otherwise = Nothing
  where
    is cmd y = (cmdChar : cmd) `isPrefixOf` y

-- Write a "PRIVMSG" message to the server
privmsg :: String -> Net ()
privmsg x = write $ "PRIVMSG " ++ (chan ++ " :" ++ x)

----------------------
-- HELPER FUNCTIONS --
----------------------

-- Write a message to the server, and also to stdout
write :: String -> Net ()
write s = do
  h <- asks socket
  io $ hPrintf h "%s\r\n" s
  io $ printf    "> %s\n" s

-- Lift an IO action into the Net monad
io :: IO a -> Net a
io = liftIO
