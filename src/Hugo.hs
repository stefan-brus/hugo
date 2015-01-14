-- Hugo bot module
-- Contains the logic for handling various commands

module Hugo where

import Control.Applicative
import Control.Arrow
import Control.Monad.Random
import Control.Monad.State

import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Time.Clock
import Data.Time.Format

import qualified Database.Redis as R

import System.Exit
import System.IO
import System.Locale
import System.Time

import Text.Printf

import Calc
import qualified Config
import Phrase
import Words

-----------
-- TYPES --
-----------

data Bot = Bot {
  socket :: Handle,
  starttime :: ClockTime,
  randomgen :: StdGen,
  phrasebook :: Phrasebook,
  learning :: Bool,
  responding :: Bool,
  channel :: String
}

type Net = StateT Bot IO

data Command =
    Quit
  | Id
  | Uptime
  | Nonsense
  | Roll Integer Integer
  | Phrase
  | Learn
  | Status
  | Respond
  | IsFriday
  | Calc String
  deriving (Show)

data IrcMsgMeta =
    ServerMeta {
      from :: String,
      magic :: String
    }
  | MessageMeta {
      nickname :: String,
      username :: String,
      cmdname :: String,
      chn :: String,
      magic :: String
    }
  deriving (Show)

data IrcMessage = IrcMessage IrcMsgMeta String deriving (Show)

--------------------------
-- HUGO LOGIC FUNCTIONS --
--------------------------

-- Evaluate an IRC message in string format
eval :: String -> Net ()
eval x = case parseMessage x of
  Just msg -> evalIrcMsg msg
  Nothing -> return ()

-- Evaluate an IRC message, skip server messages for now
evalIrcMsg :: IrcMessage -> Net ()
evalIrcMsg (IrcMessage (ServerMeta _ _) _) = return ()
evalIrcMsg (IrcMessage (MessageMeta nname _ _ ch _) msg) = do
  modify $ updateChannel ch
  evalUserMsg msg nname

-- Evaluate an IRC user message
evalUserMsg :: String -> String -> Net ()
evalUserMsg x n = case command x of
  Just Quit -> quit n
  Just Id -> id' x
  Just Uptime -> uptime
  Just Nonsense -> nonsense
  Just (Roll d s) -> roll d s
  Just Phrase -> phrase
  Just Learn -> changeLearnState n
  Just Status -> status
  Just Respond -> changeRespondState n
  Just IsFriday -> friday
  Just (Calc s) -> calc s
  Nothing -> do
    isResponding <- gets responding
    when isResponding $ reply x n
    isLearning <- gets learning
    if isLearning && (not $ [Config.cmdChar] `isPrefixOf` x) then analyze x else return ()

-- Reply to a user who said hugo's nick
reply :: String -> String -> Net ()
reply x n
  | Config.nick `isInfixOf` x = do
      pb <- gets phrasebook
      p <- io $ evalRandIO $ generatePhrase pb
      privmsg $ n ++ ", " ++ printPhrase p
  | otherwise = return ()

-- Analyze a sentence - update the phrasebook with the words
analyze :: String -> Net ()
analyze x = do
  pb <- gets phrasebook
  let pb' = learnString pb x
  modify $ updatePhrasebook pb'
  io $ savePhrasebook pb

-- Save the bot state to redis and quit. Admin command.
quit :: String -> Net ()
quit n = if n /= Config.admin then scold n else do
  pb <- gets phrasebook
  io $ savePhrasebook pb
  write "QUIT :Exiting" >> io (exitWith ExitSuccess)

-- Say the string after the id command
id' :: String -> Net ()
id' x = privmsg $ drop 4 x

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
roll :: Integer -> Integer -> Net ()
roll x y = if x > 10000 || y > 10000 then privmsg "A limit of 10000 has been imposed on both arguments, meatbag." else do
  g <- gets randomgen
  let (res,g') = foldr (\_ (acc,gen) -> let (r,gen') = randomR (1,y) gen in (r:acc,gen')) ([],g) [1..x]
  action $ "rolls " ++ (show x) ++ "d" ++ (show y) ++ ": " ++ (show res)
  modify $ updateRndGen g'

-- Generate a phrase from the phrasebook
phrase :: Net ()
phrase = do
  pb <- gets phrasebook
  p <- io $ evalRandIO $ generatePhrase pb
  privmsg $ printPhrase p

-- Turns phrase learning on or off. Admin command.
changeLearnState :: String -> Net ()
changeLearnState n = if n /= Config.admin then scold n else do
  l <- gets learning
  let msg = if l then "Dectivating language module, meatbag." else "Activating language module, meatbag."
  privmsg msg
  modify $ updateLearnState (not l)

-- Turns responding on or off. Admin command.
changeRespondState :: String -> Net ()
changeRespondState n = if n /= Config.admin then scold n else do
  r <- gets responding
  let msg = if r then "Ignoring meatbag addressments." else "Attempting to converse with meatbags."
  privmsg msg
  modify $ updateRespondState (not r)

-- Print the status of the bot
status :: Net ()
status = do
  l <- gets learning
  ps <- M.size <$> gets phrasebook
  privmsg $ if l then "I am currently learning meatbag speak." else "I am not currently learning."
  privmsg $ "I know " ++ show ps ++ " pairs of meatbag words."

-- Check if it's friday
friday :: Net ()
friday = do
  day <- io today
  let msg = if day == "Friday" then "It's friday, meatbags!" else "Negative, meatbag. It is " ++ day ++ "."
  privmsg msg

-- Calculate and print the result of the given input string
calc :: String -> Net ()
calc s = do
  case calculate s of
    Left err -> privmsg $ "Invalid meatbag mathematics: " ++ (unwords . lines . show) err
    Right res -> privmsg $ show res

-- See what command the given string is, or nothing if it isn't one
command :: String -> Maybe Command
command x
  | is "quit" = Just Quit
  | is "id" = Just Id
  | is "uptime" = Just Uptime
  | is "nonsense" = Just Nonsense
  | is "roll" = parseRoll . safeTail $ words x
  | is "phrase" = Just Phrase
  | is "learn" = Just Learn
  | is "status" = Just Status
  | is "respond" = Just Respond
  | is "friday" = Just IsFriday
  | is "calc" = Just . Calc $ drop (length ":calc") x
  | otherwise = Nothing
  where
    is cmd = (Config.cmdChar : cmd) `isPrefixOf` x

-- Save the phrasebook to redis
savePhrasebook :: Phrasebook -> IO ()
savePhrasebook pb = do
  conn <- R.connect Config.dbInfo
  R.runRedis conn $ do
    _ <- R.set (B.pack "hugo:phrasebook") (B.pack $ show pb)
    return ()

-- Read the phrasebook from redis
readPhrasebook :: IO Phrasebook
readPhrasebook = do
  conn <- R.connect Config.dbInfo
  pb <- R.runRedis conn $ do
    resStr <- R.get $ B.pack "hugo:phrasebook"
    return $ case resStr of
      Right (Just str) -> read $ B.unpack str
      _ -> M.empty
  return pb

----------------------
-- HELPER FUNCTIONS --
----------------------

-- Write a "PRIVMSG" message to the server
privmsg :: String -> Net ()
privmsg x = do
  c <- gets channel
  write $ "PRIVMSG " ++ (c ++ " :" ++ (trim x))

-- Write an action privmsg to the server
action :: String -> Net ()
action x = privmsg $ ['\x01'] ++ "ACTION " ++ x ++ ['\x01']

-- Scold a user for trying to execute an admin command
scold :: String -> Net ()
scold n = privmsg $ n ++ ", you are not an admin meatbag, meatbag."

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
updateRndGen g (Bot h t _ p l r c) = Bot h t g p l r c

-- Update the state with a new phrasebook
updatePhrasebook :: Phrasebook -> Bot -> Bot
updatePhrasebook p (Bot h t g _ l r c) = Bot h t g p l r c

-- Updates the phrase learning state
updateLearnState :: Bool -> Bot -> Bot
updateLearnState l (Bot h t g p _ r c) = Bot h t g p l r c

-- Updates the responding state
updateRespondState :: Bool -> Bot -> Bot
updateRespondState r (Bot h t g p l _ c) = Bot h t g p l r c

-- Updates the current channel
updateChannel :: String -> Bot -> Bot
updateChannel c (Bot h t g p l r _) = Bot h t g p l r c

-- Generate a sentence of random length, with random words
randomSentence :: StdGen -> (String, StdGen)
randomSentence g = let (res,g'') = randomWords len g' in (unwords res, g'')
  where
    (len,g') = randomR (2,Config.nsnsMaxLen) g

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
takeNumber :: String -> Maybe (Integer, String)
takeNumber [] = Nothing
takeNumber xs = let (digits,rest) = span isDigit xs in if length digits > 0 then Just (read digits,rest) else Nothing

-- Trim a string down to the configured max length, keep the action symbol at the end if it exists
trim :: String -> String
trim xs
  | length xs > maxLen = take (maxLen - 3) xs ++ "..." ++ if last xs == '\x01' then ['\x01'] else []
  | otherwise = xs
  where
    maxLen = fromInteger Config.msgMaxLen

-- Parse an IRC message and maybe generate an instance of IrcMessage
parseMessage :: String -> Maybe IrcMessage
parseMessage s = do
  (_,s') <- takeUntil ':' s
  h <- safeHead $ words $ drop 1 s'
  if '!' `elem` h then ircMessage s' else serverMessage s'
  where
    serverMessage :: String -> Maybe IrcMessage
    serverMessage m = do
      (addr,m') <- takeUntil ' ' m
      (mgc,m'') <- takeUntil ':' m'
      return $ IrcMessage (ServerMeta addr mgc) m''
    ircMessage :: String -> Maybe IrcMessage
    ircMessage m = do
      (nname,m') <- takeUntil '!' m
      (uname,m'') <- takeUntil ' ' m'
      (cname,m''') <- takeUntil ' ' m''
      (ch,m'''') <- takeUntil ' ' m'''
      (mgc,m''''') <- takeUntil ':' m''''
      return $ IrcMessage (MessageMeta nname uname cname ch mgc) m'''''

-- Take until the given char is reached
takeUntil :: Char -> String -> Maybe (String,String)
takeUntil _ [] = Nothing
takeUntil c s = let res = takeWhile (/= c) s in Just (res,drop (length res + 1) s)

-- Find out what day it is
today :: IO String
today = (formatTime defaultTimeLocale "%A" . utctDay) <$> getCurrentTime
