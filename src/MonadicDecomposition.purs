module Krestia.MonadicDecomposition where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.State (StateT, get, lift, put, runStateT)
import Control.MonadZero (guard)
import Data.Array (any, fromFoldable, reverse)
import Data.Foldable (oneOf)
import Data.List (List(..), many)
import Data.Maybe (Maybe, isJust)
import Data.String (length, take, toUpper)
import Data.String.Utils (endsWith)
import Data.Tuple (Tuple(..), fst)
import Krestia.Decomposition (DecomposedWord(..), isNonterminalDigit, isTerminalDigit)
import Krestia.Phonotactics (isValidWord)
import Krestia.WordTypes (Inflection(..), WI(..), WordType(..), baseTypeOf, predicativeToDefinite, prefixToPostfix, suffixes)
import Partial.Unsafe (unsafeCrashWith)

-- Types

type DecompositionStep = StateT String Maybe

-- Constants

validInflections :: Array (DecompositionStep Inflection)
validInflections = map decomposeWith suffixes

-- Helpers

decomposeWith :: WI -> DecompositionStep Inflection
decomposeWith (WI suffix inflection _) = do
   previousRemaining <- get
   let
      isSuffix = endsWith suffix previousRemaining
      remainingWord = take (length previousRemaining - length suffix) previousRemaining
      isRemainingWordValid = isValidWord remainingWord
   guard (isSuffix && isRemainingWordValid)
   put remainingWord
   pure inflection

readBaseType :: WordType -> (String -> Boolean) -> DecompositionStep WordType
readBaseType wordType typeGuard = do
   get >>= typeGuard >>> guard
   put ""
   pure wordType

checkInflections :: List Inflection -> WordType -> Boolean
checkInflections inflections wordType = unsafeCrashWith "TODO: checkInflections"

isAssociativePostfixed :: String -> Boolean
isAssociativePostfixed word =
   any ((flip endsWith) word) ["dri", "gri", "dru", "gru"] || endsWith "r" word

isPI :: String -> Boolean
isPI = predicativeToDefinite >>> isJust

pToD :: String -> String
pToD word =
   if endsWith word "aa" then
      take (length word - 2) word <> "a"
   else if endsWith word "o" then
      take (length word - 1) word <> "e"
   else
      take (length word - 1) word <> "i"

-- Readers

readInflections :: DecompositionStep (List Inflection)
readInflections = many (oneOf validInflections)

readTerminalDigit :: DecompositionStep WordType
readTerminalDigit = readBaseType TerminalDigit isTerminalDigit

readNonterminalDigit :: DecompositionStep WordType
readNonterminalDigit = readBaseType NonterminalDigit isNonterminalDigit

readName :: DecompositionStep WordType
readName = readBaseType Name \word ->
   let
      first = take 1 word
   in
   toUpper first == first

readFixedWord :: DecompositionStep WordType
readFixedWord = readName <|> readTerminalDigit <|> readNonterminalDigit

readPostfixed :: DecompositionStep Inflection
readPostfixed = do
   word <- get
   guard (isAssociativePostfixed word)
   put (prefixToPostfix word)
   pure Postfixed

readPI :: DecompositionStep Inflection
readPI = do
   word <- get
   guard (isPI word)
   put (pToD word)
   pure PredicativeIdentity

readBaseWord :: DecompositionStep WordType
readBaseWord = do
   word <- get
   baseType <- lift (baseTypeOf word)
   put ""
   pure baseType

readCommonWord :: DecompositionStep (Tuple (List Inflection) WordType)
readCommonWord = (do
   inflections <- many (oneOf validInflections <|> readPostfixed <|> readPI)
   baseWordType <- readBaseWord
   pure (Tuple inflections baseWordType)) <|> (do
      baseWord <- readBaseWord
      pure (Tuple Nil baseWord))

-- Runners

runInflections :: String -> Maybe (Tuple (List Inflection) String)
runInflections = runStateT readInflections

runFixedWord :: String -> Maybe (Tuple WordType String)
runFixedWord = runStateT readFixedWord

runBaseWord :: String -> Maybe (Tuple WordType String)
runBaseWord = runStateT readBaseWord

runPostfixed :: String -> Maybe (Tuple Inflection String)
runPostfixed = runStateT readPostfixed

runPI :: String -> Maybe (Tuple Inflection String)
runPI = runStateT readPI

runCommonWord :: String -> Maybe (Tuple (Tuple (List Inflection) WordType) String)
runCommonWord = runStateT readCommonWord

-- Main function

decompose :: String -> Maybe DecomposedWord
decompose input =
   let
      fixedWord =
         runFixedWord input
            <#> fst
            <#> { steps: [], baseType: _, baseWord: input }
            <#> DecomposedWord
      commonWord = do
         Tuple inflections baseWord <- runInflections input
         Tuple baseType _ <- runBaseWord baseWord
         guard (checkInflections inflections baseType)
         pure (DecomposedWord { steps: reverse (fromFoldable inflections), baseType, baseWord })
   in
   fixedWord <|> commonWord