{-# OPTIONS_GHC -Wall #-}

-- Hugo bot module
-- Contains the logic for handling various commands

module Hugo where

import Control.Arrow
import Control.Monad.State

import Data.List

import System.Exit
import System.IO
import System.Random
import System.Time

import Text.Printf

import Config
import Words

-----------
-- TYPES --
-----------

data Bot = Bot { socket :: Handle, starttime :: ClockTime, randomgen :: StdGen }

type Net = StateT Bot IO

data Command =
    Quit
  | Id
  | Uptime
  | Nonsense

--------------------------
-- HUGO LOGIC FUNCTIONS --
--------------------------

-- Check what the given command is and evaluate it
eval :: String -> Net ()
eval x = case command x of
  Just Quit -> quit
  Just Id -> id' x
  Just Uptime -> uptime
  Just Nonsense -> nonsense
  Nothing -> return ()

-- Quit!
quit :: Net ()
quit = write "QUIT :Exiting" >> io (exitWith ExitSuccess)

-- Say the string after the id command
id' :: String -> Net ()
id' = privmsg . drop 4

-- Say what the uptime is
uptime :: Net ()
uptime = do
  now <- io getClockTime
  zero <- gets starttime
  privmsg . prettyTime $ diffClockTimes now zero

-- Spout some nonsense
nonsense :: Net ()
nonsense = do
  g <- gets randomgen
  let (msg,g') = randomSentence g
  privmsg msg
  modify $ updateRndGen g'

-- See what command the given string is, or nothing if it isn't one
command :: String -> Maybe Command
command x
  | is "quit" x = Just Quit
  | is "id" x = Just Id
  | is "uptime" x = Just Uptime
  | is "nonsense" x = Just Nonsense
  | otherwise = Nothing
  where
    is cmd y = (cmdChar : cmd) `isPrefixOf` y

----------------------
-- HELPER FUNCTIONS --
----------------------

-- Write a "PRIVMSG" message to the server
privmsg :: String -> Net ()
privmsg x = write $ "PRIVMSG " ++ (chan ++ " :" ++ x)

-- Write a message to the server, and also to stdout
write :: String -> Net ()
write s = do
  h <- gets socket
  io $ hPrintf h "%s\r\n" s
  io $ printf    "> %s\n" s

-- Lift an IO action into the Net monad
io :: IO a -> Net a
io = liftIO

-- Prettify a time difference
prettyTime :: TimeDiff -> String
prettyTime td = unwords $ map (uncurry (++) . first show) $ if null diffs then [(0,"s")] else diffs
  where
    diffs = filter ((/= 0) . fst) $ reverse $ snd $ foldl merge (tdSec td,[]) metrics
    merge (tot,acc) (sec,typ) = let (sec',tot') = divMod tot sec in (tot',(sec',typ):acc)
    metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]

-- Update the state with a new random generator
updateRndGen :: StdGen -> Bot -> Bot
updateRndGen g (Bot h t _) = Bot h t g

-- Generate a sentence of random length, with random words
randomSentence :: StdGen -> (String, StdGen)
randomSentence g = let (res,g'') = randomWords len g' in (unwords res, g'')
  where
    (len,g') = randomR (2,nsnsMaxLen) g

-- Generate a list of random words
randomWords :: Integer -> StdGen -> ([String], StdGen)
randomWords 0 g = ([], g)
randomWords n g = let (rst,g'') = randomWords (n - 1) g' in ((allWords !! idx) : rst, g'')
  where
    (idx,g') = randomR (1,wordCount) g
