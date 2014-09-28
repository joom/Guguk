module Guguk.TurkishAlphabet
where

import qualified Data.Text as T
import qualified Data.Char as C

import qualified Guguk.Morphology.Phonetics as Ph

alphabet :: String
alphabet = "abcçdefgğhıijklmnoöprsştuüvyz"

-- | Handles the vowels with circumflex, even though they are not officially
-- included in the alphabet.
characters :: String
characters = alphabet ++ "âêîû"

toUpper :: Char -> Char
toUpper 'ı' = 'I'
toUpper 'i' = 'İ'
toUpper c   = C.toUpper c

toLower :: Char -> Char
toLower 'I' = 'ı'
toLower 'İ' = 'i'
toLower c   = C.toLower c

getPhonemes :: Char -> [Ph.Phoneme]
getPhonemes c = Ph.getBySurfaceForm $ T.singleton $ toLower c

isVowel :: Char -> Bool
isVowel c = any Ph.isVowel (getPhonemes c)

isConsonant :: Char -> Bool
isConsonant c = any Ph.isConsonant (getPhonemes c)

isLongVowel :: Char -> Bool
isLongVowel c = isVowel c && elem Ph.Long (map Ph.vowelLength (getPhonemes c))
