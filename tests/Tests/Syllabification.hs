module Tests.Syllabification where

import Guguk.Syllabification
import Test.HUnit

tests :: Test
tests = TestList $ map TestCase
  [
    -- Syllable tests
    assertEqual "Syllabify \"ece\""
    ["e", "ce"]
    (syllabify "ece")

  , assertEqual "Syllabify \"birbirlerine\""
    ["bir", "bir", "le", "ri", "ne"]
    (syllabify "birbirlerine")

  , assertEqual "Syllabify \"kafiyelendirmiştir\""
    ["ka","fi","ye","len","dir","miş","tir"]
    (syllabify "kafiyelendirmiştir")

  , assertEqual "Syllabify \"hayatında\""
    ["ha","ya","tın","da"]
    (syllabify "hayatında")

  , assertEqual "Syllabify \"kraliçe\""
    ["kra", "li", "çe"]
    (syllabify "kraliçe")

    -- One of the longest possible words in Turkish. Just because you can.
  , assertEqual "Syllabify \"muvaffakiyetsizleştiricileştiriveremeyebileceklerimizdenmişsinizcesine\""
    ["mu","vaf","fa","ki","yet","siz","leş","ti","ri","ci","leş","ti","ri","ve","re","me","ye","bi","le","cek","le","ri","miz","den","miş","si","niz","ce","si","ne"]
    (syllabify "muvaffakiyetsizleştiricileştiriveremeyebileceklerimizdenmişsinizcesine")

    -- Troublesome words syllabification tests
  , assertEqual "Syllabify \"arktik\""
    ["ark", "tik"]
    (syllabify "arktik")

  , assertEqual "Syllabify \"kontratak\""
    ["kont","ra","tak"]
    (syllabify "kontratak")

  , assertEqual "Syllabify \"tundra\""
    ["tund", "ra"]
    (syllabify "tundra")

  , assertEqual "Syllabify \"sürpriz\""
    ["sürp", "riz"]
    (syllabify "sürpriz")

  , assertEqual "Syllabify \"bandrol\""
    ["band", "rol"]
    (syllabify "bandrol")

  , assertEqual "Syllabify \"program\""
    ["prog", "ram"]
    (syllabify "program")

  , assertEqual "Syllabify \"santral\""
    ["sant", "ral"]
    (syllabify "santral")

  , assertEqual "Syllabify \"elektrik\""
    ["e", "lek", "trik"]
    (syllabify "elektrik")

  , assertEqual "Syllabify \"stratosfer\""
    ["stra", "tos", "fer"]
    (syllabify "stratosfer")

  , assertEqual "Syllabify \"yeşilimtrak\""
    ["ye", "şi", "lim", "trak"]
    (syllabify "yeşilimtrak")

    -- Syllabification with apostrophe tests
  , assertEqual "Syllabify \"mef'ûlü\""
    ["mef","û","lü"]
    (syllabify "mef'ûlü")

  , assertEqual "Syllabify \"müfte'ilün\""
    ["müf","te","i","lün"]
    (syllabify "müfte'ilün")
  ]
