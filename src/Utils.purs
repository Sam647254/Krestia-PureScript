module Krestia.Utils where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Error
   = DecomposeError String
   | InvalidInflectionError String String
   | InvalidBaseWordError String
   | Other String

derive instance eqError :: Eq Error
derive instance genericError :: Generic Error _

instance showError :: Show Error where
   show = genericShow