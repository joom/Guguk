module Tests.TurkishAlphabet where

import Guguk.TurkishAlphabet
import Test.HUnit

tests :: Test
tests = TestList $ map TestCase
  [
    -- Case conversion tests
    -- A sentence that contains all special Turkish characters.
    assertEqual "map toUpper \"Pijamalı hasta \"yağız\" şoföre 'çabucak' güvendi.\""
    "PİJAMALI HASTA \"YAĞIZ\" ŞOFÖRE 'ÇABUCAK' GÜVENDİ."
    (map toUpper "Pijamalı hasta \"yağız\" şoföre 'çabucak' güvendi.")

  , assertEqual "map toLower \"PİJAMALI HASTA \"YAĞIZ\" ŞOFÖRE 'ÇABUCAK' GÜVENDİ.\""
    "pijamalı hasta \"yağız\" şoföre 'çabucak' güvendi."
    (map toLower "PİJAMALI HASTA \"YAĞIZ\" ŞOFÖRE 'ÇABUCAK' GÜVENDİ.")

    -- Check letter property
  , assertEqual "isVowel 'a'"     True  (isVowel 'a')
  , assertEqual "isVowel 'e'"     True  (isVowel 'e')
  , assertEqual "isVowel 'f'"     False (isVowel 'f')
  , assertEqual "isVowel 'ç'"     False (isVowel 'ç')
  , assertEqual "isConsonant 'ş'" True  (isConsonant 'ş')
  , assertEqual "isConsonant 'v'" True  (isConsonant 'v')
  , assertEqual "isConsonant 'ı'" False (isConsonant 'ı')
  , assertEqual "isConsonant 'ü'" False (isConsonant 'ü')
  , assertEqual "isLongVowel 'â'" True  (isLongVowel 'â')
  , assertEqual "isLongVowel 'e'" False (isLongVowel 'e')
  , assertEqual "isLongVowel 'g'" False (isLongVowel 'g')
  ]
