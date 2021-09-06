module Krestia.Dictionary where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (elem, filter)
import Data.Either (Either(..), fromRight)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe)
import Data.String (splitAt, toUpper)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Generic (class Decode, F, Options, decodeJSON, defaultOptions, genericDecode)
import Krestia.Decomposition (DecomposedWord(..), decompose)
import Krestia.WordTypes (Inflection(..), WI(..), WordType(..), suffixes)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

normalizeField :: String -> String
normalizeField name =
   let { before, after } = splitAt 1 name in
   (toUpper before) <> after

data DictionaryWord
   = Noun Substantivo
   | Verb Verbo
   | ModifierWord Modifanto
   | SpecialWord SpecialaVorto

newtype DictionaryIndex = DictionaryIndex (Map String DictionaryWord)

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

toWordtype :: Char -> WordType
toWordtype t = case t of
   'K' -> CountableNoun
   'k' -> UncountableNoun
   'M' -> Verb0
   't' -> Verb1
   'T' -> Verb12
   'D' -> Verb123
   'n' -> Verb2
   'O' -> Verb23
   'Y' -> Verb3
   'N' -> Verb13
   'L' -> Record
   'E' -> CountableAssociativeNoun
   'P' -> UncountableAssociativeNoun
   'Q' -> Placeholder
   'F' -> Name
   'C' -> TerminalDigit
   'c' -> NonterminalDigit
   _ -> unsafeCrashWith ("Unknown word type: " <> (show t))

toInflection :: Char -> Array Inflection
toInflection i = case i of
   'D' -> [Definite]
   'H' -> [Possession]
   'P' -> [Progressive]
   'p' -> [Perfect]
   'I' -> [Intention]
   'd' -> [Desiderative]
   'E' -> [PredicativeIdentity]
   'A' -> [AttributiveIdentityPrefix, AttributiveIdentityPostfix]
   'h' -> [Possession, Possessive0]
   'i' -> [Imperative]
   '1' -> [Argument1]
   '2' -> [Argument2]
   '3' -> [Argument3]
   'e' -> [Existence]
   't' -> [Hortative]
   'T' -> [Translative, Translative0]
   'Ĝ' -> [Gerund]
   'ĝ' -> [SpecificGerund]
   'U' -> [Partial1]
   'J' -> [Partial2]
   'O' -> [Partial3]
   'S' -> [SingleForm]
   'R' -> [Reflection, Reflection0, Reflection1, Reflection3]
   '4' -> [Shift2]
   '5' -> [Shift3]
   'o' -> [Optative]
   'K' -> [Quality]
   'n' -> [Hypothetical]
   'X' -> [Detached]
   'F' -> [NameI]
   'C' -> [DigitI]
   '&' -> [Predicate]
   _ -> unsafeCrashWith ("Unknown inflection: " <> (show i))

getWordFromDictionaryWord :: DictionaryWord -> String
getWordFromDictionaryWord word = case word of
   Noun (Substantivo noun) -> noun.vorto
   Verb (Verbo verb) -> verb.vorto
   ModifierWord (Modifanto modifier) -> modifier.vorto
   SpecialWord (SpecialaVorto specialWord) -> specialWord.vorto

getRootsFromDictionaryWord :: DictionaryWord -> Array String
getRootsFromDictionaryWord word = case word of
   Noun (Substantivo noun) -> noun.radikoj
   Verb (Verbo verb) -> verb.radikoj
   ModifierWord (Modifanto modifier) -> modifier.radikoj
   SpecialWord (SpecialaVorto specialWord) -> specialWord.radikoj

getMeaningfromDictionaryWord :: DictionaryWord -> String
getMeaningfromDictionaryWord word = case word of
   Noun (Substantivo noun) -> noun.signifo
   Verb (Verbo verb) -> verb.signifo
   ModifierWord (Modifanto modifier) -> modifier.signifo
   SpecialWord (SpecialaVorto specialWord) -> specialWord.signifo

getGlossFromDictionaryWord :: DictionaryWord -> String
getGlossFromDictionaryWord word = case word of
   Noun (Substantivo noun) -> noun.gloso
   Verb (Verbo verb) -> verb.gloso
   ModifierWord (Modifanto modifier) -> modifier.gloso
   SpecialWord (SpecialaVorto specialWord) -> specialWord.gloso

wordTypeOf :: DictionaryWord -> WordType
wordTypeOf dictionaryWord =
   let
      word = getWordFromDictionaryWord dictionaryWord
      (DecomposedWord decomposedWord) = unsafePartial (fromRight (decompose word))
   in
   decomposedWord.baseType

inflectedFormsOf :: DictionaryWord -> Map Inflection String
inflectedFormsOf dictionaryWord =
   let
      wordtype = wordTypeOf dictionaryWord
      word = getWordFromDictionaryWord dictionaryWord
      applicableSuffixes = filter (\(WI s i types) -> wordtype `elem` types) suffixes
      entries = map (\(WI s i _) -> Tuple i (word <> s)) applicableSuffixes
   in
   fromFoldable entries