module Test.MonadicDecomposeSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Krestia.Decomposition (DecomposedWord(..))
import Krestia.MonadicDecomposition (decompose)
import Krestia.WordTypes (Inflection(..), WordType(..))
import Test.Unit (TestSuite, Test, suite, test)
import Test.Unit.Assert (equal)

testBaseWord :: String -> WordType -> Test
testBaseWord word wordtype =
   decompose word `equal`
      Just (DecomposedWord {steps: [], baseType: wordtype, baseWord: word})

testInflectedWord :: String -> String -> WordType -> Array Inflection -> Test
testInflectedWord word baseWord wordtype inflections =
   Just (DecomposedWord {steps: inflections, baseType: wordtype, baseWord}) `equal`
      decompose word

testInvalidWord :: String -> Test
testInvalidWord = decompose >>> equal Nothing

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
   
   test "can read inflected words" do
      testInflectedWord "vilkares" "vilka" CountableNoun [Possessive]
      testInflectedWord "todrensa" "todre" CountableAssociativeNoun [Possession]
      testInflectedWord "setela" "set" Verb12 [Intention]
      testInflectedWord "betetie" "bet" Verb12 [Argument1]
      testInflectedWord "kunaa" "kuna" UncountableNoun [PredicativeIdentity]
      testInflectedWord "vilkaa" "vilka" CountableNoun [PredicativeIdentity]
      testInflectedWord "todro" "todre" CountableAssociativeNoun [PredicativeIdentity]

   test "can read multi-inflected words" do
      testInflectedWord "vilkarese" "vilka" CountableNoun [Possessive, Hypothetical]
      testInflectedWord "betetiega" "bet" Verb12 [Argument1, AttributiveIdentityPostfix]
      testInflectedWord "betetio" "bet" Verb12 [Argument1, PredicativeIdentity]
      testInflectedWord "todru" "todre" CountableAssociativeNoun [Postfixed, PredicativeIdentity]
      testInflectedWord "meperilasetieremela" "mep" Verb123
         [Argument3, Translative, Argument1, Possessive0, Intention]
   
   test "can reject invalid words" do
      _ <- traverse testInvalidWord ["t", "tatt", "ah"]
      pure unit