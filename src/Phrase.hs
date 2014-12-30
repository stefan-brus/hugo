-- Phrase learner & generator module

module Phrase where

import Control.Monad.Random

import Data.Char
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S

-----------
-- TYPES --
-----------

type Word = String

type Pair = (Word,Word)

type Phrase = [Pair]

type Paths = S.Set Pair

type Phrasebook = M.Map Pair Paths

----------------------------
-- PHRASE LOGIC FUNCTIONS --
----------------------------

-- Update a given phrasebook with the given string
learnString :: Phrasebook -> String -> Phrasebook
learnString pb = foldl learnPhrase pb . toPhrases

-- Update a given phrasebook with the given phrase
learnPhrase :: Phrasebook -> Phrase -> Phrasebook
learnPhrase pb = fst . foldl learn (pb,("",""))
  where
    learn :: (Phrasebook, Pair) -> Pair -> (Phrasebook, Pair)
    learn (res,("","")) p = (res,p)
    learn (res,pp) p = (learnPairLink res pp p,p)

-- Update a given phrasebook with a link from the first pair to the second
learnPairLink :: Phrasebook -> Pair -> Pair -> Phrasebook
learnPairLink pb p1 p2 =
  case M.lookup p1 pb of
    Just ps -> M.insert p1 (S.insert p2 ps) pb
    Nothing -> M.insert p1 (S.singleton p2) pb

-- Generate a phrase using the given phrasebook and random generator
generatePhrase :: (RandomGen g) => Phrasebook -> Rand g Phrase
generatePhrase pb | M.size pb == 0 = return []
generatePhrase pb = do
  let len = M.size pb
  idx <- getRandomR (0,len - 1)
  generate (M.toList pb !! idx)
  where
    generate :: (RandomGen g) => (Pair, Paths) -> Rand g Phrase
    generate (p@(_,""),_) = return [p]
    generate (p,ps) = do
      let len = S.size ps
      idx <- getRandomR (0,len - 1)
      let nxt = S.toList ps !! idx
      rst <- case M.lookup nxt pb of
        Just ps' -> generate (nxt,ps')
        Nothing -> return [nxt]
      return (p:rst)

----------------------
-- HELPER FUNCTIONS --
----------------------

-- Strip non-alphanumerics and convert a string to all lowercase, making it a linguistic word
toWord :: String -> Word
toWord = map toLower . filter isAlpha

-- Turn a string into a list of phrases. Splits the string on delimiters.
toPhrases :: String -> [Phrase]
toPhrases = map toPhrase . splitWhen (`elem` delims)

-- Turn a string into a list of word pairs
toPhrase :: String -> Phrase
toPhrase = phraseify . map toWord . words
  where
    phraseify :: [Word] -> Phrase
    phraseify [] = []
    phraseify [w] = [(w,"")]
    phraseify (w1:w2:ws) = (w1,w2) : phraseify (w2:ws)

-- Turn a phrase into a string
printPhrase :: Phrase -> String
printPhrase [] = ""
printPhrase [(w,"")] = w
printPhrase ((w,_):ws) = w ++ " " ++ printPhrase ws

-- Available phrase delimiters
delims :: String
delims = ".!?"
