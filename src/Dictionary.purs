module Krestia.Dictionary where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.String (splitAt, toUpper)
import Effect (Effect)
import Foreign.Generic (class Decode, F, Options, decodeJSON, defaultOptions, genericDecode)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafeCrashWith)

normalizeField :: String -> String
normalizeField name =
   let { before, after } = splitAt 1 name in
   (toUpper before) <> after

data Substantivo = Substantivo
   { vorto :: String
   , signifo :: String
   , gloso :: String
   , radikoj :: Array String
   , noto :: Maybe String
   , plenaFormo :: Maybe String
   }

decodeOptions :: Options
decodeOptions = defaultOptions { fieldTransform = normalizeField, unwrapSingleConstructors = true }

derive instance genericSubstantivo :: Generic Substantivo _

instance showSubstantivo :: Show Substantivo where
   show = genericShow

instance decodeSubstantivo :: Decode Substantivo where
   decode = genericDecode decodeOptions

data Verbo = Verbo 
   { vorto :: String
   , signifo :: String
   , gloso :: String
   , radikoj :: Array String
   , noto :: Maybe String
   , plenaFormo :: Maybe String
   , argumentajNotoj :: Array (Maybe String)
   , frazaSignifo :: String
   }

derive instance genericVerbo :: Generic Verbo _

instance showVerbo :: Show Verbo where
   show = genericShow

instance decodeVerbo :: Decode Verbo where
   decode = genericDecode decodeOptions

data Modifanto = Modifanto
   { vorto :: String
   , signifo :: String
   , gloso :: String
   , radikoj :: Array String
   , noto :: Maybe String
   , modifeblajTipoj :: Array String
   , aldonaĵajTipoj :: Array String
   , aldonaĵajNotoj :: Array (Maybe String)
   }

derive instance genericModifanto :: Generic Modifanto _

instance showModifanto :: Show Modifanto where
   show = genericShow

instance decodeModifanto :: Decode Modifanto where
   decode = genericDecode decodeOptions

data SpecialaVorto = SpecialaVorto
   { vorto :: String
   , signifo :: String
   , gloso :: String
   , radikoj :: Array String
   , noto :: Maybe String
   }

derive instance genericSpecialaVorto :: Generic SpecialaVorto _

instance showSpecialaVorto :: Show SpecialaVorto where
   show = genericShow

instance decodeSpecialaVorto :: Decode SpecialaVorto where
   decode = genericDecode decodeOptions

data JsonVortaro = JsonVortaro
   { substantivoj :: Array Substantivo
   , verboj :: Array Verbo
   , modifantoj :: Array Modifanto
   , specialajVortoj :: Array SpecialaVorto
   }

derive instance genericJsonVortaro :: Generic JsonVortaro _

instance showJsonVortaro :: Show JsonVortaro where
   show = genericShow

instance decodeJsonVortaro :: Decode JsonVortaro where
   decode = genericDecode decodeOptions

loadDictionary :: String -> F JsonVortaro
loadDictionary = decodeJSON

testLoadDictionary :: Effect JsonVortaro
testLoadDictionary = do
   text <- readTextFile UTF8 "novaVortaro.json"
   case runExcept (loadDictionary text) of
      Left errors -> unsafeCrashWith (show errors)
      Right dictionary -> pure dictionary