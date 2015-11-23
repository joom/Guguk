-- Based on https://hackage.haskell.org/package/fullstop-0.1.4
-- Copyright (C) 2009 Eric Kow <eric.kow@gmail.com> - MIT License
-- Modified to work on Turkish.

-- module Guguk.Tokenization.SentenceBoundary ( segment ) where
module Guguk.Tokenization.SentenceBoundary  where

import Data.Char
import Data.List
import Data.List.Split
--import qualified Data.Text as T

import Guguk.Tokenization.SentenceBoundary.Ignore

-- | 'segment' @s@ splits @s@ into a list of sentences.
--
--   It looks for punctuation characters that indicate an
--   end-of-sentence and tries to ignore some uses of
--   puncuation which do not correspond to ends of sentences
--
--   It's a good idea to view the source code to this module,
--   especially the test suite.
--
--   I imagine this sort of task is actually ambiguous and that
--   you actually won't be able to write an exact segmenter.
--
--   It may be a good idea to go see the literature on how to do
--   segmentation right, maybe implement something which returns
--   the N most probable segmentations instead.
segment :: String -> [String]
segment = map (dropWhile isSpace) . squish . breakup

-- | Helper function to segment
breakup :: String -> [String]
breakup = split
          . condense       -- "huh?!"
          . dropFinalBlank -- strings that end with terminator
          . keepDelimsR    -- we want to preserve terminators
          $ oneOf ".?!:"   -- stop punctuation

-- TODO: handle ":"
-- TODO: handle "..." usage to show omitted words
squish :: [String] -> [String]
squish = squishBy (\x _ -> any (`isWordSuffixOf` x) (abbrs ++ initials))
       . squishBy (\_ y -> not (startsWithSpace y))
       . squishBy (\x _ -> looksLikeAnInitial (dropWhile isSpace x))
       . squishBy (\x y -> endsWithDigit x  && startsWithDigit y)
       . squishBy (\x _ -> endsWithDigit x)          -- Turkish ordinal num.
       . squishBy (\x _ -> all isUpper (lastWord x)) -- Uppercase Abbrv.

looksLikeAnInitial :: String -> Bool
looksLikeAnInitial x =
  case reverse x of
  ['.', i]    -> isUpper i
  ('.':i:s:_) -> isSpace s && isUpper i
  _ -> False

startsW :: (t -> Bool) -> [t] -> Bool
startsW _ [] = False
startsW f (x:_) = f x

startsWithDigit :: String -> Bool
startsWithDigit = startsW isDigit

startsWithSpace :: String -> Bool
startsWithSpace = startsW isSpace

endsWithDigit :: String -> Bool
endsWithDigit xs =
  case reverse xs of
    ('.':x:_) -> isDigit x
    _ -> False

lastWord :: String -> String
lastWord = filter (not . isPunc) . last . splitOn " "
  where isPunc = (`elem` (" .-" :: String))

-- | This is *not* (map concat . groupBy f) because the latter
--   just checks equality on the first element of each group.
--   We, on the other hand, want to check by the nearest neighbour
squishBy :: (String -> String-> Bool) -> [String] -> [String]
squishBy _ []     = []
squishBy eq (y:ys) = map concat (helper [] y ys)
 where
  helper acc x0 [] = [assemble acc x0]
  helper acc x0 (x1:xs) =
   if x0 `eq` x1
      then helper (x0:acc) x1 xs
      else assemble acc x0 : helper [] x1 xs
  assemble acc x0 = reverse (x0 : acc)

isWordSuffixOf :: String -> String -> Bool
x  `isWordSuffixOf` y | x `isSuffixOf` y =
 case drop (length x) (reverse y) of
   []    -> True -- x == y
   (z:_) -> not (isAlpha z)
_  `isWordSuffixOf` _ = False
