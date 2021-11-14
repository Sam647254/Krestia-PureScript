module Krestia.MonadicDecomposition where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.State (StateT, get, lift, put, runStateT)
import Control.MonadZero (guard)
import Data.Array (any, catMaybes, concat, elem, fromFoldable, head)
import Data.Foldable (oneOf)
import Data.List (List(..), many, null, (:))
import Data.Maybe (Maybe, fromMaybe, isJust)
import Data.String (length, take, toUpper)
import Data.String.Utils (endsWith)
import Data.Tuple (Tuple(..), fst)
import Krestia.Decomposition (DecomposedWord(..), isNonterminalDigit, isTerminalDigit)
import Krestia.Phonotactics (isValidWord)
import Krestia.WordTypes (Inflection(..), WI(..), WordType(..), baseTypeOf, behaviourOf, canUsePI, predicativeToDefinite, prefixToPostfix, suffixes)
import Partial.Unsafe (unsafeCrashWith)

-- Types

type DecompositionT = StateT String Maybe
data DecompositionStep
   = BaseStep WordType
   | InflectionStep Inflection

-- Constants

validInflections :: Array (DecompositionT Inflection)
validInflections = map decomposeWith suffixes

step :: Array (DecompositionT Inflection)
step = validInflections <> [readPostfixed, readPI]

-- Helpers

decomposeWith :: WI -> DecompositionT Inflection
decomposeWith (WI suffix inflection _) = do
   previousRemaining <- get
   let
      isSuffix = endsWith suffix previousRemaining
      remainingWord = take (length previousRemaining - length suffix) previousRemaining
      isRemainingWordValid = isValidWord remainingWord
   guard (isSuffix && isRemainingWordValid)
   put remainingWord
   pure inflection

readBaseType :: WordType -> (String -> Boolean) -> DecompositionT WordType
readBaseType wordType typeGuard = do
   get >>= typeGuard >>> guard
   put ""
   pure wordType

isAssociativePostfixed :: String -> Boolean
isAssociativePostfixed word =
   any ((flip endsWith) word) ["dri", "gri", "dru", "gru"] || endsWith "r" word

isPI :: String -> Boolean
isPI = predicativeToDefinite >>> isJust

pToD :: String -> String
pToD word =
   if endsWith "aa" word then
      take (length word - 2) word <> "a"
   else if endsWith "o" word then
      take (length word - 1) word <> "e"
   else
      take (length word - 1) word <> "i"

isValidNextInflection :: WordType -> Inflection -> Boolean
isValidNextInflection wordType inflection =
   let
      hasValidSuffix =
         any
            (\(WI _ nextInflection validBaseTypes) ->
               inflection == nextInflection && wordType `elem` validBaseTypes)
            suffixes
      validPI = inflection == PredicativeIdentity && (canUsePI wordType)
   in
   hasValidSuffix || validPI

v :: WordType -> Inflection -> List Inflection -> Boolean
v baseType inflection Nil = isValidNextInflection baseType inflection
v baseType inflection (nextInflection : rest) = (do
   (Tuple wordtype isTerminal) <- behaviourOf baseType inflection
   guard (isValidNextInflection wordtype nextInflection && (not isTerminal || null rest))
   pure (v wordtype nextInflection rest)) # fromMaybe false

validDerivation :: Tuple (List Inflection) String -> Maybe DecomposedWord
validDerivation (Tuple inflections baseWord) = do
   (Tuple baseType _) <- runBaseWord baseWord
   case inflections of
      Nil -> unsafeCrashWith "Invalid state"
      (first : rest) -> do
         guard (isValidNextInflection baseType first && v baseType first rest)
         pure (DecomposedWord { steps: fromFoldable inflections, baseType, baseWord })

-- Readers

readTerminalDigit :: DecompositionT WordType
readTerminalDigit = readBaseType TerminalDigit isTerminalDigit

readNonterminalDigit :: DecompositionT WordType
readNonterminalDigit = readBaseType NonterminalDigit isNonterminalDigit

readName :: DecompositionT WordType
readName = readBaseType Name \word ->
   let
      first = take 1 word
   in
   toUpper first == first

readFixedWord :: DecompositionT WordType
readFixedWord = readName <|> readTerminalDigit <|> readNonterminalDigit

readPostfixed :: DecompositionT Inflection
readPostfixed = do
   word <- get
   guard (isAssociativePostfixed word)
   put (prefixToPostfix word)
   pure Postfixed

readPI :: DecompositionT Inflection
readPI = do
   word <- get
   guard (isPI word)
   put (pToD word)
   pure PredicativeIdentity

readBaseWord :: DecompositionT WordType
readBaseWord = do
   word <- get
   baseType <- lift (baseTypeOf word)
   put ""
   pure baseType

readCommonWord :: DecompositionT (Tuple (List Inflection) WordType)
readCommonWord = (do
   inflections <- many (oneOf validInflections <|> readPostfixed <|> readPI)
   baseWordType <- readBaseWord
   pure (Tuple inflections baseWordType)) <|> (do
      baseWord <- readBaseWord
      pure (Tuple Nil baseWord))

-- Runners

runStep :: String -> Array (Tuple Inflection String)
runStep word = map ((flip runStateT) word) step # catMaybes

runFixedWord :: String -> Maybe (Tuple WordType String)
runFixedWord = runStateT readFixedWord

runBaseWord :: String -> Maybe (Tuple WordType String)
runBaseWord = runStateT readBaseWord

-- Main function

decompose :: String -> Maybe DecomposedWord
decompose input =
   let
      addPreviousSteps :: List Inflection -> Tuple Inflection String -> Tuple (List Inflection) String
      addPreviousSteps inflections (Tuple inflection word) = (Tuple (inflection : inflections) word)

      runStep' :: Tuple (List Inflection) String -> Array (Tuple (List Inflection) String)
      runStep' (Tuple inflectionsSoFar remainingWord) =
         let
            nextStep = runStep remainingWord
            newList = map (addPreviousSteps inflectionsSoFar) nextStep
            nextStep' = newList <#> runStep' # concat
         in
         nextStep' <|> newList


      fixedWord =
         runFixedWord input
            <#> fst
            <#> { steps: [], baseType: _, baseWord: input }
            <#> DecomposedWord
      commonWord =
         (runStep' (Tuple Nil input) <#> validDerivation # catMaybes # head)
            <|> (runBaseWord input
                  <#> fst 
                  <#> { steps: [], baseType: _, baseWord: input }
                  <#> DecomposedWord)

   in
   fixedWord <|> commonWord