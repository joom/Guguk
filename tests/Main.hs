module Main where

import Test.HUnit

import qualified Tests.Syllabification as Syl
import qualified Tests.TurkishAlphabet as Alph
import qualified Tests.Morphology.Phonology as Ph
import qualified Tests.Tokenization.SentenceBoundary as Sent

tests :: [Test]
tests = [ Syl.tests, Alph.tests, Ph.tests, Sent.tests ]

runTests ::  IO ()
runTests = do
  _ <- mapM runTestTT tests
  return ()

main :: IO ()
main = runTests
