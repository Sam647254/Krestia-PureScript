module Test.Main where

import Prelude

import Effect (Effect)
import PhonotacticsSpec as P
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
   P.spec