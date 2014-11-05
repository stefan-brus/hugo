{-# OPTIONS_GHC -Wall #-}

-- Phrase learner & generator module

module Phrase where

import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S

import System.Random

-----------
-- TYPES --
-----------

type Word = String

type Phrase = [Word]

type Paths = S.Set Word

type Phrasebook = M.Map Word Paths

----------------------------
-- PHRASE LOGIC FUNCTIONS --
----------------------------

-- Update a given phrasebook with the given string
learnString :: Phrasebook -> String -> Phrasebook
learnString pb = learnPhrase pb . map toWord . words

-- Update a given phrasebook with the givenphrase
learnPhrase :: Phrasebook -> Phrase -> Phrasebook
learnPhrase pb = fst . foldl learn (pb,"")
  where
    learn :: (Phrasebook, Word) -> Word -> (Phrasebook, Word)
    learn (res,"") w = (learnWordLink res w "",w)
    learn (res,pw) w = (learnWordLink (learnWordLink res pw w) w "", w)

-- Update a given phrasebook with a link from the first word to the second
learnWordLink :: Phrasebook -> Word -> Word -> Phrasebook
learnWordLink pb w1 w2 =
  case M.lookup w1 pb of
    Just ws -> M.insert w1 (S.insert w2 ws) pb
    Nothing -> M.insert w1 (S.singleton w2) pb

-- Generate a phrase using the given phrasebook and random generator
generatePhrase :: Phrasebook -> StdGen -> (Phrase, StdGen)
generatePhrase pb g | M.size pb == 0 = ([],g)
generatePhrase pb g =
  let
    len = M.size pb
    (idx,g') = randomR (0,if len > 0 then len - 1 else len) g
    (res,g'') = generate (M.toList pb !! idx) g'
  in
    (mergeDelims res,g'')
  where
    mergeDelims :: Phrase -> Phrase
    mergeDelims = reverse . foldl merge []
      where
        merge :: Phrase -> Word -> Phrase
        merge [] w = [w]
        merge res@(p:ps) w = if isDelim w then (p ++ w) : ps else w : res
    generate :: (Word, Paths) -> StdGen -> (Phrase, StdGen)
    generate ("", ps) gen = let (res,gen') = choose ps "" gen in (res,gen')
    generate (w,ps) gen | S.size ps == 0 = ([w],gen)
    generate (w,ps) gen =
      let
        len = S.size ps
        (idx,gen') = randomR (0,if len > 0 then len - 1 else len) gen
        w' = S.toList ps !! idx
        ps' = case M.lookup w' pb of
                Just paths -> paths
                Nothing -> S.empty
        (rest,gen'') = generate (w',ps') gen'
      in
        (w:rest,gen'')

    choose :: Paths -> Word -> StdGen -> (Phrase, StdGen)
    choose ps w gen = case randomR (0,19) gen :: (Int,StdGen) of
      (0,gen') -> ([],gen')
      (n,gen') | n `elem` [1..18] -> generate (w,ps) gen'
      (_,gen') -> let
                   (delim,gen'') = chooseDelim gen'
                   (res,gen''') = generatePhrase pb gen''
                 in
                   ([delim] : res, gen''')
    chooseDelim :: StdGen -> (Char, StdGen)

    chooseDelim gen = let (idx,gen') = randomR (0, length delims - 1) gen in (delims !! idx,gen')

    isDelim :: Word -> Bool
    isDelim [c] = c `elem` delims
    isDelim _ = False

    delims = ".,!?:;"

----------------------
-- HELPER FUNCTIONS --
----------------------

-- Strip non-alphanumerics and convert a string to all lowercase, making it a linguistic word
toWord :: String -> Word
toWord = map toLower . filter isAlpha

-- Convert a string to a phrase
toPaths :: String -> Paths
toPaths = S.fromList . words
