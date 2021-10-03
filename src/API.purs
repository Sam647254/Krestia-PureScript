module Krestia.API where

import Prelude

import Data.Either (Either(..), fromRight)
import Data.Generic.Rep (class Generic)
import Data.List as L
import Data.Map (filter, lookup, toUnfoldable, values)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.String.Utils (startsWith, words)
import Data.Tuple (Tuple(..))
import Foreign.Generic (class Encode, Options, defaultOptions, genericEncode)
import Foreign.Object (Object, fromFoldable)
import Krestia.Decomposition (DecomposedWord(..), decompose)
import Krestia.Dictionary (DictionaryIndex(..), DictionaryWord(..), Modifanto(..), Verbo(..), getGlossFromDictionaryWord, getMeaningfromDictionaryWord, getRootsFromDictionaryWord, getWordFromDictionaryWord, inflectedFormsOf, wordTypeOf)
import Krestia.Phonotactics (splitIntoSyllables)
import Krestia.WordTypes (WordType, Inflection)
import Partial.Unsafe (unsafePartial)

encodeOptions :: Options
encodeOptions = defaultOptions { unwrapSingleConstructors = true }

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
   encode = genericEncode encodeOptions

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

data WordMeaning = WordMeaning
   { word :: String
   , meaning :: String
   }

derive instance genericWordMeaning :: Generic WordMeaning _

instance encodeWordMeaning :: Encode WordMeaning where
   encode = genericEncode encodeOptions

data GlossResult = GlossResult
   { word :: String
   , baseWord :: String
   , glossMeaning :: String
   , inflectionSteps :: Array String
   }

derive instance genericGlossResult :: Generic GlossResult _

instance encodeGlossResult :: Encode GlossResult where
   encode = genericEncode encodeOptions

data SearchResult = SearchResult
   { decomposedWord :: Maybe String
   , fullWord :: Maybe String
   , results :: Array WordMeaning
   , glossResults :: Maybe (Array GlossResult)
   , inflectionSteps :: Maybe (Array String)
   , numberResult :: Maybe Number
   }

derive instance genericSearchResult :: Generic SearchResult _

instance encodeSearchResult :: Encode SearchResult where
   encode = genericEncode encodeOptions

search :: DictionaryIndex -> String -> SearchResult
search (DictionaryIndex index) query =
   let
      queryWords = words query
      searchResults =
         filter (\entry -> contains (Pattern query) (getWordFromDictionaryWord entry) ||
            contains (Pattern query) (getMeaningfromDictionaryWord entry)) index
            # values
            # map (\result -> WordMeaning {word: getWordFromDictionaryWord result,
               meaning: getMeaningfromDictionaryWord result})
      Tuple decomposedWord inflectionSteps =
         case queryWords of
         [word] ->
            let decomposedWord = decompose word in
            case decomposedWord of
               Right (DecomposedWord validWord) ->
                  Tuple (Just validWord.baseWord) (Just (map show validWord.steps))
               Left _ ->
                  Tuple Nothing Nothing
         _ -> Tuple Nothing Nothing
   in
   SearchResult
      { decomposedWord
      , fullWord: Nothing
      , results: L.toUnfoldable searchResults
      , glossResults: Nothing
      , inflectionSteps
      , numberResult: Nothing
      }

relevanceOf :: String -> WordMeaning -> Int
relevanceOf query (WordMeaning result) =
   if query == result.word then
      0
   else if startsWith query result.word then
      1
   else if query == result.meaning then
      2
   else if startsWith query result.meaning then
      3
   else if contains (Pattern query) result.meaning then
      4
   else
      100