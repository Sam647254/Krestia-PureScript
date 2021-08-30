module Krestia.Dictionary where

import Prelude

import Data.Array (concatMap, length, unsafeIndex) as A
import Data.Array.Partial (tail)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (length)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Krestia.Decomposition (DecomposedWord(..), decompose, isVerb)
import Krestia.Utils (Error(..))
import Krestia.WordTypes (Inflection(..), WordType(..))
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

data DictionaryWord = DictionaryWord
   { word :: String
   , meaning :: String
   , glossMeaning :: String
   , roots :: Array String
   , notes :: Maybe String
   , slotMeanings :: Maybe (Array String)
   , modifies :: Maybe (Array WordType)
   , attachments :: Maybe (Array Inflection)
   }

derive instance genericDictionaryWord :: Generic DictionaryWord _

instance showDictionaryWord :: Show DictionaryWord where
  show = genericShow

data Dictionary = Dictionary
   { words :: Array DictionaryWord }

derive instance genericDictionary :: Generic Dictionary _

instance showDictionary :: Show Dictionary where
   show = genericShow

loadDictionary :: String -> Either Error Dictionary
loadDictionary contents = do
   let dictionaryLines = lines contents
   traverse loadWord dictionaryLines
      # map (\words -> (Dictionary {words: words}))

loadWord :: String -> Either Error DictionaryWord
loadWord line = do
   let
      parts = split (Pattern "|") line
      word = parts `index` 0
   DecomposedWord decomposedWord <- decompose word
   if length decomposedWord.steps > 1 then
      Left (Other (word <> " is not a dictionary word"))
   else do
      let
         Tuple meaning slots =
            if isVerb (DecomposedWord decomposedWord) then
               let meaningParts = split (Pattern "^") (parts `index` 1) in
               Tuple (meaningParts `index` 0) (Just (unsafePartial (tail meaningParts)))
            else
               Tuple (parts `index` 1) Nothing
         glossMeaning = parts `index` 2
         roots = if (parts `index` 3) == "" then [] else split (Pattern ",") (parts `index` 3)
         notes = if (parts `index` 4) == "" then Nothing else Just (parts `index` 4)
         modifies =
            if A.length parts >= 6 then
               Just (map toWordtype (toCharArray (parts `index` 5)))
            else
               Nothing
         attachments =
            if A.length parts == 7 then
               Just (A.concatMap toInflection (toCharArray (parts `index` 6)))
            else
               Nothing
      pure (DictionaryWord
         { word: word
         , meaning: meaning
         , glossMeaning: glossMeaning
         , roots: roots
         , notes: notes
         , slotMeanings: slots
         , modifies: modifies
         , attachments: attachments
         })

toWordtype :: Char -> WordType
toWordtype t = case t of
   'N' -> CountableNoun
   'n' -> UncountableNoun
   '0' -> Verb0
   '1' -> Verb1
   '2' -> Verb12
   '3' -> Verb123
   '4' -> Verb2
   '5' -> Verb23
   '6' -> Verb3
   '7' -> Verb13
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
   '@' -> [NameI]
   'C' -> [DigitI]
   '&' -> [Predicate]
   _ -> unsafeCrashWith ("Unknown inflection: " <> (show i))

index :: forall a. Array a -> Int -> a
index array i = unsafePartial (A.unsafeIndex array i)