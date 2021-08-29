module Krestia.Phonotactics where

import Prelude

import Data.Array (toUnfoldable)
import Data.Either (Either(..), isRight)
import Data.List (List(..), (:))
import Data.List as L
import Data.String (Pattern(..), Replacement(..), replace, toLower)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Krestia.Utils (Error(..))

data Letter
   = Consonant Char
   | Vowel Char

derive instance eqLetter :: Eq Letter

normalize :: String -> String
normalize =
   replace (Pattern "aa") (Replacement "ɒ")
   >>> replace (Pattern "sh") (Replacement "ʃ")
   >>> toLower

toLetter :: Char -> Letter
toLetter 'a' = Vowel 'a'
toLetter 'e' = Vowel 'e'
toLetter 'i' = Vowel 'i'
toLetter 'ɒ' = Vowel 'ɒ'
toLetter 'o' = Vowel 'o'
toLetter 'u' = Vowel 'u'
toLetter c = Consonant c

categorize :: String -> List Letter
categorize = toCharArray >>> map toLetter >>> toUnfoldable

isValidWord :: String -> Boolean
isValidWord = splitIntoSyllables >>> isRight

splitIntoSyllables :: String -> Either Error (Array String)
splitIntoSyllables word =
   let
      wordLetters = normalize word # categorize

      guard :: Boolean -> String -> Either Error Unit
      guard true _ = Right unit
      guard false message = Left (Other message)

      divide :: Boolean -> List Letter -> Either Error (Array String)
      divide isStart letters =
         case letters of
            Nil -> do
               -- guard (not isStart) "La eniro estas malplena"
               pure []
            Vowel v : Nil -> Right [fromCharArray [v]]
            Consonant k1 : Consonant k2 : Vowel v : Consonant kf : Nil -> do
               guard isStart "Vorto ne rajtas komenci per du Consonantj"
               pure [fromCharArray [k1, k2, v, kf]]
            Consonant k1 : Consonant k2 : Vowel v : Nil -> do
               guard isStart "Vorto ne rajtas komenci per du Consonantj"
               pure [fromCharArray [k1, k2, v]]
            Consonant k1 : Vowel v : Consonant kf : Nil -> Right [fromCharArray [k1, v, kf]]
            Consonant k1 : Vowel v : Nil -> Right [fromCharArray [k1, v]]
            Vowel v : Consonant kf : Nil -> Right [fromCharArray [v, kf]]
            Consonant k1 : Consonant k2 : Vowel v :
               Consonant kf : Consonant kk2 : restantaj -> do
               guard isStart "Vorto ne rajtas komenci per du Consonantoj"
               restanta <- divide false (Consonant kk2 : restantaj)
               pure (fromCharArray [k1, k2, v, kf] : (toUnfoldable restanta) # L.toUnfoldable)
            Consonant k1 : Consonant k2 : Vowel v : Consonant kf :
               Vowel v2 : restantaj -> do
               guard isStart "Vorto ne rajtas komenci per du Consonantj"
               restanta <- divide false (Consonant kf : Vowel v2 : restantaj)
               pure (fromCharArray [k1, k2, v] : (toUnfoldable restanta) # L.toUnfoldable)
            Consonant k1 : Consonant k2 : Vowel v : Vowel v2 : restantaj -> do
               guard isStart "Vorto ne rajtas komenci per du Consonantj"
               guard (v /= v2) "Vowel ne rajtas aperi dufoje"
               restanta <- divide false (Vowel v2 : restantaj)
               pure (fromCharArray [k1, k2, v] : (toUnfoldable restanta) # L.toUnfoldable)
            Consonant k1 : Vowel v : Consonant kf : Consonant kk2 : restantaj -> do
               restanta <- divide false (Consonant kk2 : restantaj)
               pure (fromCharArray [k1, v, kf] : (toUnfoldable restanta) # L.toUnfoldable)
            (Consonant k1 : Vowel v : Consonant kk2 : Vowel v2 : restantaj) -> do
               restanta <- divide false (Consonant kk2 : Vowel v2 : restantaj)
               pure (fromCharArray [k1, v] : (toUnfoldable restanta) # L.toUnfoldable)
            (Consonant k1 : Vowel v : Vowel v2 : restantaj) -> do
               guard (v /= v2) "Vowel ne rajtas aperi dufoje"
               restanta <- divide false (Vowel v2 : restantaj)
               pure (fromCharArray [k1, v] : (toUnfoldable restanta) # L.toUnfoldable)
            (Vowel v : Consonant kf : Consonant kk2 : restantaj) -> do
               restanta <- divide false (Consonant kk2 : restantaj)
               pure (fromCharArray [v, kf] : (toUnfoldable restanta) # L.toUnfoldable)
            (Vowel v : Consonant kf : Vowel v2 : restantaj) -> do
               restanta <- divide false (Consonant kf : Vowel v2 : restantaj)
               pure (fromCharArray [v] : (toUnfoldable restanta) # L.toUnfoldable)
            (Vowel v : Vowel v2 : restantaj) -> do
               guard (v /= v2) "Vowel ne rajtas aperi dufoje"
               restanta <- divide false (Vowel v2 : restantaj)
               pure (fromCharArray [v] : (toUnfoldable restanta) # L.toUnfoldable)
            _ -> Left (Other "Nevalida vorto")
   in
   divide true wordLetters