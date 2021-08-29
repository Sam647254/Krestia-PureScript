module Test.Main where

import Prelude

import Effect (Effect)
import Test.DecomposeSpec as D
import PhonotacticsSpec as P
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
   D.spec
   P.spec