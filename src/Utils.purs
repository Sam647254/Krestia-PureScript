module Krestia.Utils where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Foreign.Generic (class Encode, class GenericEncode, defaultOptions, genericEncode)

data Error
   = DecomposeError String
   | InvalidInflectionError String String
   | InvalidBaseWordError String
   | Other String

derive instance eqError :: Eq Error
derive instance genericError :: Generic Error _

instance showError :: Show Error where
   show = genericShow

newtype EncodableEither a b = EncodableEither (Either a b)

derive instance genericEncodableEither :: Generic (EncodableEither a b) _
derive instance newtypeEncodableEither :: Newtype (EncodableEither a b) _

instance encodeEncodableEither ::
   (Generic a rep, Generic b rep2, GenericEncode rep, GenericEncode rep2) =>
      Encode (EncodableEither a b) where
   encode (EncodableEither ee) = case ee of
      Left left -> genericEncode defaultOptions left
      Right right -> genericEncode defaultOptions right