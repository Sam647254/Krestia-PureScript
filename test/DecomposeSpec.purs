module Test.DecomposeSpec where

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
spec = suite "Base words" do
   test "can read nouns" do
      testBaseWord "vilka" CountableNoun