module Test.DecomposeSpec where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..))
import Krestia.Decomposition (DecomposedWord(..), decompose)
import Krestia.WordTypes (WordType(..))
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (equal)

testBaseWord :: String -> WordType -> Test
testBaseWord word wordtype =
   decompose word `equal`
      Right (DecomposedWord {steps: Nil, baseType: wordtype, baseWord: word})

spec :: TestSuite
spec = suite "Decompose" do
   test "can read base words" do
      testBaseWord "vilka" CountableNoun
      testBaseWord "edre" CountableAssociativeNoun
      testBaseWord "set" Verb12
      testBaseWord "telit" Verb12
      testBaseWord "bep" Verb123
      testBaseWord "pelip" Verb123
      testBaseWord "Krestia" Name
      testBaseWord "hise" Placeholder