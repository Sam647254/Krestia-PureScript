module Krestia.API where

import Prelude

import Data.Either (fromRight)
import Data.Generic.Rep (class Generic)
import Data.Map (lookup, toUnfoldable)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Foreign.Generic (class Encode, defaultOptions, genericEncode)
import Foreign.Object (Object, fromFoldable)
-- import Krestia.Dictionary (Dictionary(..), DictionaryWord(..), inflectedFormsOf, wordTypeOf)
import Krestia.Phonotactics (splitIntoSyllables)
import Krestia.WordTypes (WordType, Inflection)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

data WordResponse = WordResponse
   { word :: String
   , roots :: Array String
   , meaning :: String
   , glossMeaning :: String
   , wordType :: WordType
   , syllables :: Array String
   , inflectedForms :: Object String
   , slots :: Maybe (Array String)
   , contextualMeaning :: Maybe String
   , modifies :: Maybe (Array WordType)
   , attachments :: Maybe (Array Inflection)
   }

derive instance genericWordResponse :: Generic WordResponse _

instance encodeWordResponse :: Encode WordResponse where
   encode = genericEncode defaultOptions

-- findWord :: Dictionary -> String -> Maybe WordResponse
-- findWord (Dictionary dictionary) word = do
--    (DictionaryWord entry) <- lookup word dictionary.index
--    let wordtype = wordTypeOf (DictionaryWord entry)
--    unsafeCrashWith "TODO"
   -- pure (WordResponse
   --    { word: entry.word
   --    , roots: entry.roots
   --    , meaning: entry.meaning
   --    , glossMeaning: entry.glossMeaning
   --    , wordType: wordtype
   --    , syllables: unsafePartial (fromRight (splitIntoSyllables entry.word))
   --    , inflectedForms:
   --       inflectedFormsOf (DictionaryWord entry) # toUnfoldable # map (\(Tuple i w) -> (Tuple (show i) w))
   --          # fromFoldable
   --    , slots: entry.slotMeanings
   --    })