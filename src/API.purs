module Krestia.API where

import Prelude

import Data.Either (fromRight)
import Data.Generic.Rep (class Generic)
import Data.Map (lookup, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Generic (class Encode, defaultOptions, genericEncode)
import Foreign.Object (Object, fromFoldable)
import Krestia.Dictionary (DictionaryIndex(..), DictionaryWord(..), Modifanto(..), Verbo(..), getGlossFromDictionaryWord, getMeaningfromDictionaryWord, getRootsFromDictionaryWord, getWordFromDictionaryWord, inflectedFormsOf, wordTypeOf)
import Krestia.Phonotactics (splitIntoSyllables)
import Krestia.WordTypes (WordType, Inflection)
import Partial.Unsafe (unsafePartial)

data WordResponse = WordResponse
   { word :: String
   , roots :: Array String
   , meaning :: String
   , glossMeaning :: String
   , wordType :: WordType
   , syllables :: Array String
   , inflectedForms :: Object String
   , contextualMeaning :: Maybe String
   , modifies :: Maybe (Array String)
   , attachments :: Maybe (Array String)
   }

derive instance genericWordResponse :: Generic WordResponse _

instance encodeWordResponse :: Encode WordResponse where
   encode = genericEncode defaultOptions

findWord :: DictionaryIndex -> String -> Maybe WordResponse
findWord (DictionaryIndex dIndex) query = do
   entry <- lookup query dIndex
   let
      wordtype = wordTypeOf entry
      word = getWordFromDictionaryWord entry
   pure (WordResponse
      { word
      , roots: getRootsFromDictionaryWord entry
      , meaning: getMeaningfromDictionaryWord entry
      , glossMeaning: getGlossFromDictionaryWord entry
      , wordType: wordtype
      , syllables: unsafePartial (fromRight (splitIntoSyllables word))
      , inflectedForms:
         ((inflectedFormsOf entry # toUnfoldable) :: Array (Tuple Inflection String))
            # map (\(Tuple i w) -> (Tuple (show i) w))
            # fromFoldable
      , contextualMeaning: case entry of
         Verb (Verbo verb) -> Just verb.frazaSignifo
         _ -> Nothing
      , modifies: case entry of
         ModifierWord (Modifanto modifanto) -> Just modifanto.modifeblajTipoj
         _ -> Nothing
      , attachments: case entry of
         ModifierWord (Modifanto modifanto) -> Just modifanto.aldonaĵajTipoj
         _ -> Nothing
      })