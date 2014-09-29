module Main where

import Guguk.Morphology.Phonology
import Test.HUnit

tests :: Test
tests = TestList $ map TestCase
  [
    -- Vowel Harmony Tests
    assertEqual "vowelHarmony \"hoşlanıyorum\"" True  (vowelHarmony "hoşlanıyorum")
  , assertEqual "vowelHarmony \"doğruymuş\""    True  (vowelHarmony "doğruymuş")
  , assertEqual "vowelHarmony \"sözlükte\""     True  (vowelHarmony "sözlükte")
  , assertEqual "vowelHarmony \"vaziyet\""      False (vowelHarmony "vaziyet")
  , assertEqual "vowelHarmony \"asistanlık\""   False (vowelHarmony "asistanlık")
  , assertEqual "vowelHarmony \"strateji\""     False (vowelHarmony "strateji")

  , assertEqual "aTypeHarmony \"parlamento\""   'a'   (aTypeHarmony "parlamento")
  , assertEqual "aTypeHarmony \"yapımcı\""      'a'   (aTypeHarmony "yapımcı")
  , assertEqual "aTypeHarmony \"anayasa\""      'a'   (aTypeHarmony "anayasa")
  , assertEqual "aTypeHarmony \"teklifler\""    'e'   (aTypeHarmony "teklifler")
  , assertEqual "aTypeHarmony \"senatör\""      'e'   (aTypeHarmony "senatör")
  , assertEqual "aTypeHarmony \"örgüt\""        'e'   (aTypeHarmony "örgüt")

  , assertEqual "iTypeHarmony \"anayasa\""      'ı'   (iTypeHarmony "anayasa")
  , assertEqual "iTypeHarmony \"yapımcı\""      'ı'   (iTypeHarmony "yapımcı")
  , assertEqual "iTypeHarmony \"parlamento\""   'u'   (iTypeHarmony "parlamento")
  , assertEqual "iTypeHarmony \"okul\""         'u'   (iTypeHarmony "okul")
  , assertEqual "iTypeHarmony \"teklifler\""    'i'   (iTypeHarmony "teklifler")
  , assertEqual "iTypeHarmony \"özellik\""      'i'   (iTypeHarmony "özellik")
  , assertEqual "iTypeHarmony \"senatör\""      'ü'   (iTypeHarmony "senatör")
  , assertEqual "iTypeHarmony \"örgüt\""        'ü'   (iTypeHarmony "örgüt")

  , assertEqual "firstConsonantAlteration \"fıstık\" \"cı\""
    ("fıstık","çı") -- alteration happens
    (firstConsonantAlteration "fıstık" "cı")
  , assertEqual "firstConsonantAlteration \"muzip\" \"dir\""
    ("muzip","tir") -- alteration happens
    (firstConsonantAlteration "muzip" "dir")
  , assertEqual "firstConsonantAlteration \"kolay\" \"dır\""
    ("kolay","dır") --alteration doesn't happen because 'y' is not voiceless
    (firstConsonantAlteration "kolay" "dır")
  , assertEqual "firstConsonantAlteration \"kalıp\" \"lı\""
    ("kalıp","lı")  --alteration doesn't happen because 'l' is not 'c' or 'd'
    (firstConsonantAlteration "kalıp" "lı")

  , assertEqual "lastConsonantAlteration \"kitap\" \"a\""
    ("kitab","a")  -- alteration happens
    (lastConsonantAlteration "kitap" "a")
  , assertEqual "lastConsonantAlteration \"çocuk\" \"u\""
    ("çocuğ","u")  -- alteration happens
    (lastConsonantAlteration "çocuk" "u")
  , assertEqual "lastConsonantAlteration \"çocuk\" \"lu\""
    ("çocuk","lu") -- alteration doesn't happen because 'l' is not a vowel
    (lastConsonantAlteration "çocuk" "lu")
  , assertEqual "lastConsonantAlteration \"sac\" \"ı\""
    ("sac","ı")    -- alteration doesn't happen because 'c' is not a stop consonant
    (lastConsonantAlteration "sac" "ı")
  ]

runTests ::  IO ()
runTests = do
  _ <- runTestTT tests
  return ()

main :: IO ()
main = runTests
