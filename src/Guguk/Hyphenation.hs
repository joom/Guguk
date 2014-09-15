module Guguk.Hyphenation
( syllabalize, isVowel, isLongVowel,
  Syllable) where

import           Data.Char  (isAlpha, toLower)
import           Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Text as T

type Syllable = T.Text

{-|
  Returns Just x, where x character at index i
       or Nothing, is i is out of bounds
-}
charAt :: T.Text -> Int -> Maybe Char
charAt xs i = if T.length xs > i then Just (xs `T.index` i) else Nothing

{-|
  Returns True if x is a vowel,
       or False otherwise
-}
isVowel :: Char -> Bool
isVowel x = x `elem` "aeiıoöuüâêîû"

{-|
  Returns True if x is a long vowel like 'â', 'ê', 'î', or 'û',
       or False otherwise
-}
isLongVowel :: Char -> Bool
isLongVowel x = x `elem` "âêîû"

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
-- Note that apostrophe is a definite syllable separator,
-- for words of Arabic origin which containing the sound "ع".
syllabalize :: T.Text -> [Syllable]
syllabalize s
  | T.null s = []
  | '\'' `elemT` T.tail s =
    concatMap syllabalize
    [T.takeWhile (/='\'') s, T.tail $ T.dropWhile (/='\'') s]
  | isNothing firstVowelIndex = [xs]

  | any isNothing [afterVowel 1] = [xs]
  | isVowel(fromJust $ afterVowel 1) =
      substring 0 (fVI + 1) xs : syllabalize(substring (fVI + 1) len xs)

  | any isNothing [afterVowel 2] = [xs]
  | isVowel(fromJust $ afterVowel 2) =
      substring 0 (fVI + 1) xs : syllabalize(substring (fVI + 1) len xs)

  | any isNothing [afterVowel 3] = [xs]
  | isVowel(fromJust $ afterVowel 3) =
      substring 0 (fVI + 2) xs : syllabalize(substring (fVI + 2) len xs)

  | lastPart `elem` ["str", "ktr", "ntr", "nsp"] =
      substring 0 (fVI + 2) xs : syllabalize(substring (fVI + 2) len xs)
  | otherwise =
      substring 0 (fVI + 3) xs : syllabalize(substring (fVI + 3) len xs)
  where xs = (T.filter isAlpha . T.map toLower) s
        firstVowelIndex = T.findIndex isVowel xs
        fVI = fromJust firstVowelIndex
        len = T.length xs
        lastPart = substring (len + 1) (len + 4) xs
        afterVowel i = fromJust $ fmap (charAt xs . (+i)) firstVowelIndex
