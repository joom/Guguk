module Guguk.Syllabification
(syllabify, Syllable) where

import           Data.Char  (isAlpha, toLower)
import           Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Text as T

import qualified Guguk.TurkishAlphabet as Alph

type Syllable = T.Text

{-|
  Returns Just x, where x character at index i
       or Nothing, is i is out of bounds
-}
charAt :: T.Text -> Int -> Maybe Char
charAt xs i = if T.length xs > i then Just (xs `T.index` i) else Nothing

-- | Java's substring.
substring :: Int -> Int -> T.Text -> T.Text
substring x y = T.drop x . T.take y

{-|
  Returns True is the char is in the text,
       or False otherwise
-}
elemT :: Char -> T.Text -> Bool
elemT c t = isJust $ T.find (==c) t

-- | List of Turkish syllables of the given text.
-- Syllables dont contain apostrophes, they are all in lower case.
-- Note that apostrophe is a definite syllable separator
-- for words of Arabic origin which contain the sound "ع".
syllabify :: T.Text -> [Syllable]
syllabify s
  -- Base case
  | T.null s = []
  -- Split on apostrophes
  | '\'' `elemT` T.tail s = concatMap syllabify (T.splitOn "'" s)
  -- Return the same if there is no vowel
  | isNothing firstVowelIndex = [xs]

  | isNothing (afterVowel 1) = [xs]
  | Alph.isVowel(fromJust $ afterVowel 1) = handleSubstring 1

  | isNothing (afterVowel 2) = [xs]
  | Alph.isVowel(fromJust $ afterVowel 2) = handleSubstring 1

  | isNothing (afterVowel 3) = [xs]
  | Alph.isVowel(fromJust $ afterVowel 3) = handleSubstring 2

  | lastPart `elem` exceptions = handleSubstring 2
  | otherwise = handleSubstring 3
  where xs = (T.filter isAlpha . T.map toLower) s
        firstVowelIndex = T.findIndex Alph.isVowel xs
        fVI = fromJust firstVowelIndex
        len = T.length xs
        lastPart = substring 2 5 xs
        exceptions = ["str", "ktr", "mtr", "nsp"]
        afterVowel i = fromJust $ fmap (charAt xs . (+i)) firstVowelIndex
        handleSubstring n =
          substring 0 (fVI + n) xs : syllabify(substring (fVI + n) len xs)
