-- This file contains functions about the phonological and morphotactical
-- rules of the Turkish language. The rule descriptions in the Haddock comments
-- are borrowed from a zemberek-nlp document about Turkish morphology.

module Guguk.Morphology.Phonology
(vowelHarmony, aTypeHarmony, iTypeHarmony,
firstConsonantAlteration, lastConsonantAlteration) where

import Data.Maybe
import qualified Data.List as L
import qualified Data.Text as T

import qualified Guguk.TurkishAlphabet as Alph
import qualified Guguk.Phonetics as Ph
import qualified Guguk.Syllabification as Syl

same :: (Eq a) => [a] -> Bool
same xs = and $ zipWith (==) xs (tail xs)

-- Vowel harmony functions

lastSyllable :: T.Text -> String
lastSyllable w = T.unpack $ last (Syl.syllabify w)

lastVowel :: T.Text -> Char
lastVowel w = fromMaybe (error "Syllable without a vowel.")
                        (L.find Alph.isVowel $ lastSyllable w)

-- | Checks for the vowel harmony in the entire word.
vowelHarmony :: T.Text -> Bool
vowelHarmony t = same $ map Ph.vowelLocation vowelPhoneme
  where vowelList = T.unpack $ T.filter Alph.isVowel t
        vowelPhoneme = map (head . Alph.getPhonemes) vowelList

{-|
  A syllable's vowels are formed according to the last vowel
  of the word it appends to. There is an A-Type harmony;
  if last vowel of the word is a back vowel (a, ı, o, u), a is produced.
  If last vowel is a frontal vowel (e, i, ö, ü), e is produced.
  For plural suffix [-lAr] vowel is formed as these; kalem-ler, kitap-lar.
-}
aTypeHarmony :: T.Text -> Char
aTypeHarmony w = case loc of
                   Ph.Back  -> 'a'
                   Ph.Front -> 'e'
                   _        -> error "Unknown vowel location."
  where loc = Ph.vowelLocation $ head $ Ph.getBySurfaceForm $ T.singleton (lastVowel w)


{-|
  There is also an I-Type harmony;
  if last vowel is back and unrounded (a, ı), ı is produced.
  if it is back and rounded (o,u), u is produced.
  if it is frontal and unrounded (e, i), i is produced
  and lastly if it is frontal and rounded (ö, ü), ü is produced.
  For genitive suffix [-(n)In] eight outcomes are possible,
    four of them: kalem-in, kitab-ın, okul-un, üzüm-ün.
-}
iTypeHarmony :: T.Text -> Char
iTypeHarmony w = case (Ph.vowelLocation lV, Ph.vowelRoundedness lV) of
                   (Ph.Back,  Ph.Unrounded) -> 'ı'
                   (Ph.Back,  Ph.Rounded)   -> 'u'
                   (Ph.Front, Ph.Unrounded) -> 'i'
                   (Ph.Front, Ph.Rounded)   -> 'ü'
                   _ -> error "Unknown vowel location and roundedness."
  where lV = head $ Ph.getBySurfaceForm $ T.singleton $ lastVowel w

-- Consonant alteration functions
harden :: Char -> Char
harden 'c' = 'ç'
harden 'd' = 't'
harden x = x

soften :: Char -> Char
soften 'p' =  'b'
soften 'ç' =  'c'
soften 'k' =  'ğ'
soften 't' =  'd'
soften 'g' =  'ğ'
soften x = x

{-|
  When a suffix starting with some voiced consonants (c,d)
  is appended to a word ending with a voiceless consonant (f,s,t,k,ç,ş,h,p)
  first consonant of the suffix becomes voiceless (ç,t).
  fıstık->fıstık-çı (not fıstık-cı), muzip->muzip-tir (not muzip-dir)

  This function returns the two texts in a tuple,
  with the first consonant alteration done if necessary.
-}
firstConsonantAlteration :: T.Text -> T.Text -> (T.Text, T.Text)
firstConsonantAlteration w1 w2
  | isVoiceless (T.last w1) = (w1, T.cons (harden $ T.head w2) (T.tail w2))
  | otherwise = (w1, w2)
  where isVoiceless c = Ph.Voiceless == Ph.consonantVoice firstPhoneme
          where firstPhoneme = head $ Ph.getBySurfaceForm $ T.singleton c

{-|
  If last letter of a word or suffix is a stop consonant (tr: süreksiz sert sessiz),
  and a suffix that starts with a vowel is appended to that word,
  last letter changes (voicing). Changes are p-b, ç-c, k-ğ, t-d, g-ğ.
  kitap->kitab-a, pabuç->pabuc-u, çocuk->çocuğ-a, hasat->hasad-ı

  This function returns the two texts in a tuple,
  with the last consonant alteration done if necessary.
-}
lastConsonantAlteration :: T.Text -> T.Text -> (T.Text, T.Text)
lastConsonantAlteration w1 w2
  | Alph.isVowel (T.head w2) = (T.snoc (T.init w1) (soften $ T.last w1), w2)
  | otherwise = (w1, w2)
