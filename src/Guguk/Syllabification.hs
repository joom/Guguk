module Guguk.Syllabification
(syllabify, Syllable) where

import           Data.Char  (isAlpha, toLower)
import           Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Text as T

import qualified Guguk.TurkishAlphabet as Alph

type Syllable = T.Text

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
  -- now it is safe to use fVI
  | hasVowelAt (fVI+1)         = handleSubstring 1
  | hasVowelAt (fVI+2)         = handleSubstring 1
  | hasVowelAt (fVI+3)         = handleSubstring 2
  | fVI + 3 >= len             = [xs]
  | lastPart `elem` exceptions = handleSubstring 2
  | otherwise = handleSubstring 3
  where xs = (T.filter isAlpha . T.map toLower) s
        firstVowelIndex = T.findIndex Alph.isVowel xs
        fVI = fromJust firstVowelIndex
        len = T.length xs
        lastPart = substring 2 5 xs
        exceptions = ["str", "ktr", "mtr", "nsp"]
        handleSubstring n =
          substring 0 (fVI + n) xs : syllabify(substring (fVI + n) len xs)
        hasVowelAt i = i < len && Alph.isVowel (T.index xs i)
