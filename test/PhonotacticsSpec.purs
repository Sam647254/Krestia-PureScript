module PhonotacticsSpec where

import Prelude

import Data.Foldable (all)
import Krestia.Phonotactics (isValidWord)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)

spec :: TestSuite
spec = suite "Phonotactics" do
   test "can read valid base words" do
      ["Marika", "Runa", "Rejna", "Nivo", "Rini", "Silika", "Evra"]
         # all isValidWord
         # assert "should be able to read base words"