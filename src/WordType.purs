module Krestia.WordTypes where

import Prelude

import Control.Alt ((<|>))
import Data.Array (find)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (reverse, toUnfoldable, (:))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Utils (endsWith)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)

data WordType
   = CountableNoun
   | UncountableNoun
   | CountableAssociativeNoun
   | UncountableAssociativeNoun
   | Record
   | Verb1
   | Verb12
   | Verb13
   | Verb123
   | Verb2
   | Verb23
   | Verb3
   | Verb0
   | Placeholder
   | Modifier
   | Name
   | TerminalDigit
   | NonterminalDigit
   | Any

derive instance eqWordType :: Eq WordType
derive instance genericWordType :: Generic WordType _

instance showWordType :: Show WordType where
   show = genericShow

data Inflection
   = Definite
   | Possession
   | PredicativeIdentity
   | AttributiveIdentityPrefix
   | AttributiveIdentityPostfix
   | Gerund
   | SpecificGerund
   | Possessive
   | Possessive0
   | Existence
   | Translative
   | Translative0
   | Detached
   | Lone
   | Progressive
   | Perfect
   | Intention
   | Hypothetical
   | Desiderative
   | Imperative
   | Hortative
   | Optative
   | Argument1
   | Argument2
   | Argument3
   | Partial1
   | Partial2
   | Partial3
   | Shift2
   | Shift3
   | Commencement
   | Reflection
   | Reflection1
   | Reflection3
   | Reflection0
   | Postfixed
   | Quality
   | SingleForm
   | NameI
   | DigitI
   | Predicate

derive instance eqInflection :: Eq Inflection
derive instance genericInflection :: Generic Inflection _

instance showInflection :: Show Inflection where
   show = genericShow

data WI = WI String Inflection (Array WordType)

suffixes :: Array WI
suffixes =
   [ WI "va" AttributiveIdentityPrefix [CountableNoun, UncountableNoun]
   , WI "ga" AttributiveIdentityPostfix [CountableNoun, UncountableNoun]
   , WI "vra" SpecificGerund [CountableNoun, UncountableNoun]
   , WI "re" Quality [CountableNoun, UncountableNoun]
   , WI "ra" Lone [CountableNoun, UncountableNoun]
   , WI "rem" Possessive0 [CountableNoun, UncountableNoun]
   , WI "res" Possessive [CountableNoun, UncountableNoun]
   , WI "rim" Existence [CountableNoun, UncountableNoun]
   , WI "lam" Translative [CountableNoun, UncountableNoun]
   , WI "las" Translative [CountableNoun, UncountableNoun]
   
   , WI "io" Perfect [Verb0, Verb2, Verb3, Verb23]
   , WI "ia" Hypothetical [Verb0, Verb2, Verb3, Verb23]
   , WI "ela" Intention [Verb0, Verb1, Verb12, Verb13, Verb123, Verb2, Verb23, Verb3]
   , WI "ea" Gerund [Verb0, Verb12, Verb123, Verb2, Verb3, Verb23]
   , WI "ro" Perfect [Verb12, Verb123]
   , WI "o" Perfect [Verb1, Verb13]
   , WI "e" Hypothetical [Verb1, Verb13]
   , WI "ora" Desiderative [Verb1, Verb12, Verb13, Verb123]
   , WI "ea" Imperative [Verb1, Verb13]
   , WI "ri" Imperative [Verb12, Verb123]
   , WI "ie" Optative [Verb1, Verb13, Verb3]
   , WI "ra" Optative [Verb2]
   , WI "ri" Optative [Verb23]
   , WI "a" Hortative [Verb1, Verb13]
   , WI "etie" Argument1 [Verb1, Verb12, Verb13, Verb123]
   , WI "onia" Argument2 [Verb12, Verb2, Verb123, Verb23]
   , WI "eri" Argument3 [Verb123, Verb13, Verb3, Verb123]
   , WI "mea" Gerund [Verb1, Verb13]
   , WI "elim" Commencement [Verb0]
   , WI "elit" Commencement [Verb12]
   , WI "elis" Commencement [Verb1]
   , WI "elish" Commencement [Verb13]
   , WI "elip" Commencement [Verb123]
   , WI "em" Partial1 [Verb1]
   , WI "ig" Partial1 [Verb12]
   , WI "ev" Partial1 [Verb123]
   , WI "es" Partial2 [Verb12]
   , WI "am" Partial2 [Verb2]
   , WI "on" Partial2 [Verb23]
   , WI "osh" Partial2 [Verb123]
   , WI "ut" Partial3 [Verb123]
   , WI "ig" Partial3 [Verb23]
   , WI "ris" Reflection [Verb12]
   , WI "ish" Reflection [Verb123]
   , WI "es" Reflection [Verb13]
   , WI "is" Reflection1 [Verb123]
   , WI "im" Reflection0 [Verb123]
   , WI "rim" Reflection0 [Verb12]
   , WI "ret" Shift2 [Verb12]
   , WI "rop" Shift2 [Verb123]
   , WI "rup" Shift3 [Verb123]
   , WI "rosh" Shift3 [Verb13]
   , WI "riv" Shift3 [Verb23]
   ]

countableNounSuffixes :: Array String
countableNounSuffixes =
   [ "pa"
   , "pe"
   , "pi"
   , "ta"
   , "te"
   , "ti"
   , "ka"
   , "ke"
   , "ki"
   ]

uncountableNounSuffixes :: Array String
uncountableNounSuffixes =
   [ "ma"
   , "me"
   , "mi"
   , "na"
   , "ne"
   , "ni"
   ]

predicativeIdentitySuffixes :: Array (Tuple WordType (Array String))
predicativeIdentitySuffixes =
   [ Tuple CountableNoun ["paa", "po", "pu", "taa", "to", "tu", "kaa", "ko", "ku"]
   , Tuple UncountableNoun ["maa", "mo", "mu", "naa", "no", "nu"]
   , Tuple CountableAssociativeNoun ["dro", "dru"]
   , Tuple UncountableAssociativeNoun ["gro", "gru"]
   ]

predicativeToDefinite :: String -> String
predicativeToDefinite noun =
   case reverse $ Array.toUnfoldable $ toCharArray noun of
      ('a' : 'a' : r) -> fromCharArray $ toUnfoldable $ reverse ('a' : r)
      ('o' : r) -> fromCharArray $ toUnfoldable $ reverse ('e' : r)
      ('u' : r) -> fromCharArray $ toUnfoldable $ reverse ('i' : r)
      _ -> unsafeCrashWith ("Invalid noun: " <> noun)

prefixToPostfix :: String -> String
prefixToPostfix anoun =
   case reverse $ Array.toUnfoldable $ toCharArray anoun of
      ('u' : r) -> fromCharArray $ toUnfoldable $ reverse ('o' : r)
      ('i' : r) -> fromCharArray $ toUnfoldable $ reverse ('e' : r)
      ('r' : r) -> fromCharArray $ toUnfoldable $ reverse ('l' : r)
      _ -> unsafeCrashWith ("Invalid postfix word: " <> anoun)

baseTypeOf :: String -> Maybe WordType
baseTypeOf word =
   let letters = Array.toUnfoldable $ toCharArray word in
   case letters of
      ('h' : _) -> Just Placeholder
      _ ->
         case reverse letters of
            ('m' : _) -> Just Verb0
            ('s' : _) -> Just Verb1
            ('t' : _) -> Just Verb12
            ('h' : 's' : _) -> Just Verb13
            ('p' : _) -> Just Verb123
            ('l' : _) -> Just Modifier
            ('i' : 'l' : _) -> Just Record
            reversedLetters ->
               (find ((flip endsWith) word) countableNounSuffixes # map (const CountableNoun))
               <|> (find ((flip endsWith) word) uncountableNounSuffixes # map (const UncountableNoun))
               <|> (do
                  if endsWith "dre" word then
                     pure CountableAssociativeNoun
                  else if endsWith "gre" word then
                     pure UncountableAssociativeNoun
                  else
                     Nothing)

behavesLike :: Tuple Inflection WordType -> WordType -> Boolean
behavesLike _ _ = unsafeCrashWith "TODO"

usesPredicativeIdentity :: Inflection -> Maybe Inflection
usesPredicativeIdentity SpecificGerund = Just Gerund
usesPredicativeIdentity _ = Nothing