module Main where

import Guguk.Hyphenation
import Test.HUnit

tests :: Test
tests = TestList $ map TestCase
  [
    -- Syllable tests
    assertEqual "Syllabalize \"ece\""
    ["e", "ce"]
    (syllabalize "ece")

  , assertEqual "Syllabalize \"birbirlerine\""
    ["bir", "bir", "le", "ri", "ne"]
    (syllabalize "birbirlerine")

  , assertEqual "Syllabalize \"kafiyelendirmiştir\""
    ["ka","fi","ye","len","dir","miş","tir"]
    (syllabalize "kafiyelendirmiştir")

  , assertEqual "Syllabalize \"hayatında\""
    ["ha","ya","tın","da"]
    (syllabalize "hayatında")

  , assertEqual "Syllabalize \"kraliçe\""
    ["kra", "li", "çe"]
    (syllabalize "kraliçe")

    -- One of the longest possible words in Turkish. Just because you can.
  , assertEqual "Syllabalize \"muvaffakiyetsizleştiricileştiriveremeyebileceklerimizdenmişsinizcesine\""
    ["mu","vaf","fa","ki","yet","siz","leş","ti","ri","ci","leş","ti","ri","ve","re","me","ye","bi","le","cek","le","ri","miz","den","miş","si","niz","ce","si","ne"]
    (syllabalize "muvaffakiyetsizleştiricileştiriveremeyebileceklerimizdenmişsinizcesine")

    -- Troublesome words syllabalization tests
  , assertEqual "Syllabalize \"santral\""
    ["sant", "ral"]
    (syllabalize "santral")

  , assertEqual "Syllabalize \"elektrik\""
    ["e", "lek", "trik"]
    (syllabalize "elektrik")

  , assertEqual "Syllabalize \"stratosfer\""
    ["stra", "tos", "fer"]
    (syllabalize "stratosfer")

  , assertEqual "Syllabalize \"yeşilimtrak\""
    ["ye", "şi", "lim", "trak"]
    (syllabalize "yeşilimtrak")

    -- Syllabalization with apostrophe tests
  , assertEqual "Syllabalize \"mef'ûlü\""
    ["mef","û","lü"]
    (syllabalize "mef'ûlü")

  , assertEqual "Syllabalize \"müfte'ilün\""
    ["müf","te","i","lün"]
    (syllabalize "müfte'ilün")
  ]

runTests ::  IO ()
runTests = do
  _ <- runTestTT tests
  return ()

-- | For now, main will run our tests.
main :: IO ()
main = runTests
