module Guguk.Morphology.Phonology where

import qualified Data.Text as T

import qualified Guguk.TurkishAlphabet as Alph
import qualified Guguk.Phonetics as Ph

same :: (Eq a) => [a] -> Bool
same xs = and $ zipWith (==) xs (tail xs)

-- Vowel harmony functions

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
aTypeHarmony = undefined

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
iTypeHarmony = undefined


-- Consonant alteration functions

{-|
  When a suffix starting with some voiced consonants (c,d)
  is appended to a word ending with a voiceless consonant (f,s,t,k,ç,ş,h,p)
  first consonant of the suffix becomes voiceless (ç,t).
  fıstık->fıstık-çı (not fıstık-cı), muzip->muzip-tir (not muzip-dir)

  This function returns the two texts in a tuple,
  with the first consonant alteration done if necessary.
-}
firstConsonantAlteration :: T.Text -> T.Text -> (T.Text, T.Text)
firstConsonantAlteration w1 w2 = undefined

{-|
  If last letter of a word or suffix is a stop consonant (tr: süreksiz sert sessiz),
  and a suffix that starts with a vowel is appended to that word,
  last letter changes (voicing). Changes are p-b, ç-c, k-ğ, t-d, g-ğ.
  kitap->kitab-a, pabuç->pabuc-u, çocuk->çocuğ-a, hasat->hasad-ı

  This function returns the two texts in a tuple,
  with the last consonant alteration done if necessary.
-}
lastConsonantAlteration :: T.Text -> T.Text -> (T.Text, T.Text)
lastConsonantAlteration w1 w2 = undefined
