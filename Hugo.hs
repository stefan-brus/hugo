{-# OPTIONS_GHC -Wall #-}

-- Hugo bot module
-- Contains the logic for handling various commands

module Hugo where

import Control.Arrow
import Control.Monad.State

import Data.Char
import Data.List
import Data.Maybe

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
  | Roll Int Int
  deriving (Show)

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
  Just (Roll d s) -> roll d s
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

-- Roll xdy
roll :: Int -> Int -> Net ()
roll x y = do
  g <- gets randomgen
  let (res,g') = foldr (\_ (acc,gen) -> let (r,gen') = randomR (1,y) gen in (r:acc,gen')) ([],g) [1..x]
  --let (res,g') = randomRs (1,y) g :: (Int,StdGen)
  action $ "rolls " ++ (show x) ++ "d" ++ (show y) ++ ": " ++ (show res)
  modify $ updateRndGen g'

-- See what command the given string is, or nothing if it isn't one
command :: String -> Maybe Command
command x
  | is "quit" = Just Quit
  | is "id" = Just Id
  | is "uptime" = Just Uptime
  | is "nonsense" = Just Nonsense
  | is "roll" = parseRoll . safeTail $ words x
  | otherwise = Nothing
  where
    is cmd = (cmdChar : cmd) `isPrefixOf` x

----------------------
-- HELPER FUNCTIONS --
----------------------

-- Write a "PRIVMSG" message to the server
privmsg :: String -> Net ()
privmsg x = write $ "PRIVMSG " ++ (chan ++ " :" ++ (trim x))

-- Write an action privmsg to the server
action :: String -> Net ()
action x = privmsg $ ['\x01'] ++ "ACTION " ++ x ++ ['\x01']

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

-- Safe head, gives Nothing for empty list
safeHead :: [a] -> Maybe a
safeHead = listToMaybe

-- Safe tail, gives Nothing for empty list
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

-- Parse the argument to a roll command
parseRoll :: Maybe [String] -> Maybe Command
parseRoll Nothing = Just (Roll 1 6)
parseRoll (Just []) = Just (Roll 1 6)
parseRoll (Just xs) = do
  s <- safeHead xs
  (dice,s') <- takeNumber s
  (_,s'') <- takeChar 'd' s'
  (sides,_) <- takeNumber s''
  return (Roll dice sides)

-- Take the given character from the string
takeChar :: Char -> String -> Maybe (Char, String)
takeChar _ [] = Nothing
takeChar c (x:xs) = if c == x then Just (x,xs) else Nothing

-- Take a number from the string
takeNumber :: String -> Maybe (Int, String)
takeNumber [] = Nothing
takeNumber xs = let (digits,rest) = span isDigit xs in if length digits > 0 then Just (read digits,rest) else Nothing

-- Trim a string down to the configured max length, keep the action symbol at the end if it exists
trim :: String -> String
trim xs
  | length xs > maxLen = take (maxLen - 3) xs ++ "..." ++ if last xs == '\x01' then ['\x01'] else []
  | otherwise = xs
  where
    maxLen = fromInteger msgMaxLen
