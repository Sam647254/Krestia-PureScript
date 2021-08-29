module Krestia.Utils where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Error
   = Other String

derive instance genericError :: Generic Error _

instance showError :: Show Error where
   show = genericShow