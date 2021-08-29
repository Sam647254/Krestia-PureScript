module Krestia.Decomposition where

import Prelude

import Control.Alt ((<|>))
import Data.Array (any, elem, find, toUnfoldable)
import Data.Either (Either(..))
import Data.Foldable (or)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), concatMap, reverse, singleton, (:))
import Data.List.Partial (init, last)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (length, take, toUpper)
import Data.String.Utils (charAt, endsWith)
import Data.Tuple (Tuple(..))
import Krestia.Phonotactics (isValidWord)
import Krestia.Utils (Error(..))
import Krestia.WordTypes (Inflection(..), WI(..), WordType(..), baseTypeOf, behavesLike, predicativeIdentitySuffixes, predicativeToDefinite, prefixToPostfix, suffixes, usesPredicativeIdentity)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

newtype Decomposer a = Decomposer (String -> Either Error (Tuple a String))

type DecomposeResult = Tuple (List DecomposeStep) String

data DecomposeStep
   = BaseStep WordType
   | SecondaryStep Inflection (List WordType)

derive instance genericDecomposeStep :: Generic DecomposeStep _

instance showDecomposeStep :: Show DecomposeStep where
   show = genericShow

data DecomposedWord = DecomposedWord
   { steps :: List Inflection
   , baseType :: WordType
   , baseWord :: String
   }

derive instance eqDecomposedWord :: Eq DecomposedWord
derive instance genericDecomposedWord :: Generic DecomposedWord _

instance showDecomposedWord :: Show DecomposedWord where
   show = genericShow

apply :: forall a. Decomposer a -> String -> Either Error (Tuple a String)
apply (Decomposer f) = f

instance functorDecomposer :: Functor Decomposer where
   map = liftM1

instance applicativeDecomposer :: Applicative Decomposer where
   pure value = Decomposer (\input -> Right (Tuple value input))

instance applyDecomposer :: Apply Decomposer where
   apply = ap

instance bindDecomposer :: Bind Decomposer where
   bind :: forall a b. Decomposer a -> (a -> Decomposer b) -> Decomposer b
   bind decomposer decomposerF = Decomposer (\input -> do
      Tuple value rest <- apply decomposer input
      Tuple value' rest' <- apply (decomposerF value) rest
      pure (Tuple value' rest'))

instance monadDecomposer :: Monad Decomposer

isVerb :: DecomposedWord -> Boolean
isVerb (DecomposedWord word) =
   case word.baseType of
      Verb0 -> true
      Verb1 -> true
      Verb2 -> true
      Verb3 -> true
      Verb12 -> true
      Verb13 -> true
      Verb23 -> true
      Verb123 -> true
      _ -> false

isTerminalDigit :: String -> Boolean
isTerminalDigit word =
   case word of
      "mira" -> true
      "pona" -> true
      "vora" -> true
      "nona" -> true
      "tera" -> true
      "sina" -> true
      "lira" -> true
      "sona" -> true
      "kera" -> true
      "gina" -> true
      "trira" -> true
      "plora" -> true
      "klera" -> true
      "plora" -> true
      _ -> false

isNonterminalDigit :: String -> Boolean
isNonterminalDigit word = case word of
   "mi" -> true
   "po" -> true
   "vo" -> true
   "no" -> true
   "te" -> true
   "si" -> true
   "li" -> true
   "so" -> true
   "ke" -> true
   "gi" -> true
   "di" -> true
   "tri" -> true
   "plo" -> true
   "kle" -> true
   _ -> false

decomposePI :: Decomposer DecomposeStep
decomposePI = Decomposer f where
   f "" = Left (Other "Empty string")
   f word = do
      case find (\(Tuple wordtype suffixes) -> any ((flip endsWith) word) suffixes)
         predicativeIdentitySuffixes of
         Just (Tuple wordtype _) ->
            Right (Tuple (SecondaryStep PredicativeIdentity (singleton wordtype))
               (predicativeToDefinite word))
         Nothing -> Left (InvalidInflectionError word "Cannot read predicative identity")

decomposePostfixed :: Decomposer DecomposeStep
decomposePostfixed = Decomposer f where
   f "" = Left (Other "Empty string")
   f word
      | any ((flip endsWith) word) ["dri", "gri", "dru", "gru"] = do
         let
            remainingWord = prefixToPostfix word
            next =
               if endsWith "dri" word || endsWith "dru" word then
                  CountableAssociativeNoun
               else
                  UncountableAssociativeNoun
         Tuple base _ <- apply readBaseWord remainingWord
         case base of
            BaseStep wt | wt == CountableAssociativeNoun || wt == UncountableAssociativeNoun ->
               pure (Tuple (SecondaryStep Postfixed (singleton next)) remainingWord)
            _ -> unsafeCrashWith "Undefined"
      | endsWith "r" word = do
         let remainingWord = prefixToPostfix word
         Tuple base _ <- apply readBaseWord remainingWord
         pure (Tuple (SecondaryStep Postfixed (singleton Modifier)) remainingWord)
      | otherwise =
         Left (InvalidInflectionError word "Cannot read Postfixed")

readBaseWord :: Decomposer DecomposeStep
readBaseWord = Decomposer f where
   f "" = Left (Other "Empty string")
   f word = case baseTypeOf word of
      Just wordType ->
         if isValidWord word then
            Right (Tuple (BaseStep wordType) word)
         else
            Left (InvalidBaseWordError word)
      Nothing -> Left (InvalidInflectionError word "Not a base word")

readSpecialWord :: Decomposer DecomposeStep
readSpecialWord = Decomposer f where
   f word | isTerminalDigit word = Right (Tuple (BaseStep TerminalDigit) word)
   f word | isNonterminalDigit word = Right (Tuple (BaseStep NonterminalDigit) word)
   f word =
      let
         firstLetter = unsafePartial (fromJust (charAt 0 word))
      in
      if toUpper firstLetter == firstLetter then
         Right (Tuple (BaseStep Name) word)
      else
         Left (InvalidBaseWordError word)

tryFullyDecompose :: WI -> List WordType -> String -> Either Error DecomposeResult
tryFullyDecompose (WI suffix inflection wordtypes) expectedTypes word =
   let
      isCorrectInflection =
         concatMap ((\p -> map (\ewt -> p `behavesLike` ewt) expectedTypes) <<< Tuple inflection)
            (toUnfoldable wordtypes)
            # or
   in
   if endsWith suffix word && (expectedTypes == (Any : Nil) || isCorrectInflection) then do
      let remaining = take (length word - length suffix) word
      case usesPredicativeIdentity inflection of
         Just pi -> do
            Tuple steps baseWord <- fullyDecompose wordtypes remaining
            case steps of
               (SecondaryStep PredicativeIdentity _ : remainingSteps) ->
                  pure (Tuple (SecondaryStep pi (toUnfoldable wordtypes) : remainingSteps) baseWord)
               remainingSteps ->
                  pure (Tuple (SecondaryStep inflection (toUnfoldable wordtypes) : remainingSteps) baseWord)
         Nothing ->
            if inflection == Quality then do
               Tuple steps baseWord <- fullyDecompose wordtypes remaining
               case steps of
                  SecondaryStep PredicativeIdentity _ : remainingSteps ->
                     pure (Tuple (SecondaryStep Quality (toUnfoldable wordtypes) : remainingSteps) baseWord)
                  remainingSteps -> Left (InvalidInflectionError word "Quality must follow Predicative Identity")
            else do
               Tuple remainingSteps baseWord <- fullyDecompose wordtypes remaining
               pure (Tuple (SecondaryStep inflection (toUnfoldable wordtypes) : remainingSteps) baseWord)
   else
      Left (DecomposeError word)

fullyDecompose_ :: List WI -> Array WordType -> List DecomposeStep -> String ->
   Either Error DecomposeResult
fullyDecompose_ Nil wordtypes steps word = (do
   Tuple piStep remainingWord <- apply decomposePI word
   Tuple remainingSteps baseWord <- fullyDecompose wordtypes remainingWord
   pure (Tuple (piStep : remainingSteps <> steps) baseWord))
   <|> (do
      Tuple postfixedStep remainingWord <- apply decomposePostfixed word
      Tuple remainingSteps baseWord <- fullyDecompose wordtypes remainingWord
      pure (Tuple (postfixedStep : remainingSteps) baseWord))
   <|> (do
      Tuple lastStep baseWord <- apply readBaseWord word
      case lastStep of
         BaseStep wt | wt `elem` wordtypes || wordtypes == [Any] ->
            pure (Tuple (singleton lastStep) baseWord)
         _ -> Left (InvalidInflectionError word "Invalid last step"))

fullyDecompose_ (nextWI : remainingWI) wordtypes steps word =
   case tryFullyDecompose nextWI (toUnfoldable wordtypes) word of
      Right (Tuple remainingSteps baseWord) -> Right (Tuple (remainingSteps <> steps) baseWord)
      Left _ -> fullyDecompose_ remainingWI wordtypes steps word

fullyDecompose :: Array WordType -> String -> Either Error DecomposeResult
fullyDecompose wordtypes word = (do
   Tuple baseStep baseWord <- apply readSpecialWord word
   pure (Tuple (singleton baseStep) baseWord))
   <|> fullyDecompose_ (toUnfoldable suffixes) (toUnfoldable wordtypes) Nil word

decompose :: String -> Either Error DecomposedWord
decompose word = do
   Tuple steps baseWord <- fullyDecompose [Any] word
   let
      inflections =
         unsafePartial (init steps)
            # map (\s ->
               case s of
                  SecondaryStep i _ -> i
                  _ -> unsafeCrashWith "Undefined")
            # reverse
      baseStep = case unsafePartial (last steps) of
         BaseStep wt -> wt
         _ -> unsafeCrashWith "Undefined"
   pure (DecomposedWord {steps: inflections, baseType: baseStep, baseWord: baseWord})