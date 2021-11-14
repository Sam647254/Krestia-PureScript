module Krestia.WordTypes where

import Prelude

import Control.Alt ((<|>))
import Data.Array (elem, find)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (reverse, toUnfoldable, (:))
import Data.Maybe (Maybe(..))

import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Utils (endsWith)
import Data.Tuple (Tuple(..))
import Foreign.Generic (class Encode)
import Foreign.Generic.EnumEncoding (genericEncodeEnum)
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

instance encodeWordType :: Encode WordType where
   encode = genericEncodeEnum { constructorTagTransform: identity }

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
derive instance ordInflection :: Ord Inflection
derive instance genericInflection :: Generic Inflection _

instance showInflection :: Show Inflection where
   show = genericShow

instance encodeInflection :: Encode Inflection where
   encode = genericEncodeEnum { constructorTagTransform: identity }

data WI = WI String Inflection (Array WordType)

suffixes :: Array WI
suffixes =
   [ WI "nsa" Possession [CountableNoun, UncountableNoun, CountableAssociativeNoun,
      UncountableAssociativeNoun]
   , WI "va" AttributiveIdentityPrefix [CountableNoun, UncountableNoun,
      CountableAssociativeNoun, UncountableAssociativeNoun]
   , WI "ga" AttributiveIdentityPostfix [CountableNoun, UncountableNoun,
      CountableAssociativeNoun, UncountableAssociativeNoun]
   , WI "vra" SpecificGerund [CountableNoun, UncountableNoun, CountableAssociativeNoun,
      UncountableAssociativeNoun]
   , WI "re" Quality [CountableNoun, UncountableNoun, CountableAssociativeNoun,
      UncountableAssociativeNoun]
   , WI "ra" Lone [CountableNoun, UncountableNoun, CountableAssociativeNoun,
      UncountableAssociativeNoun]
   , WI "rem" Possessive0 [CountableNoun, UncountableNoun, CountableAssociativeNoun,
      UncountableAssociativeNoun]
   , WI "res" Possessive [CountableNoun, UncountableNoun, CountableAssociativeNoun,
      UncountableAssociativeNoun]
   , WI "rim" Existence [CountableNoun, UncountableNoun, CountableAssociativeNoun,
      UncountableAssociativeNoun]
   , WI "lam" Translative0 [CountableNoun, UncountableNoun, CountableAssociativeNoun,
      UncountableAssociativeNoun]
   , WI "las" Translative [CountableNoun, UncountableNoun, CountableAssociativeNoun,
      UncountableAssociativeNoun]
   
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

predicativeToDefinite :: String -> Maybe String
predicativeToDefinite noun =
   case reverse $ Array.toUnfoldable $ toCharArray noun of
      ('a' : 'a' : r) -> Just (fromCharArray $ toUnfoldable $ reverse ('a' : r))
      ('o' : r) -> Just (fromCharArray $ toUnfoldable $ reverse ('e' : r))
      ('u' : r) -> Just (fromCharArray $ toUnfoldable $ reverse ('i' : r))
      _ -> Nothing

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
behavesLike (Tuple Possessive _) Verb1 = true
behavesLike (Tuple Possessive0 _) Verb0 = true
behavesLike (Tuple Gerund _) UncountableNoun = true
behavesLike (Tuple SpecificGerund _) UncountableNoun = true
behavesLike (Tuple Quality _) UncountableNoun = true
behavesLike (Tuple Translative _) Verb1 = true
behavesLike (Tuple Translative0 _) Verb0 = true
behavesLike (Tuple Argument1 _) CountableNoun = true
behavesLike (Tuple Argument2 _) CountableNoun = true
behavesLike (Tuple Argument3 _) CountableNoun = true
behavesLike (Tuple Partial1 Verb1) Verb0 = true
behavesLike (Tuple Partial1 Verb12) Verb2 = true
behavesLike (Tuple Partial1 Verb123) Verb23 = true
behavesLike (Tuple Partial2 Verb2) Verb0 = true
behavesLike (Tuple Partial2 Verb12) Verb1 = true
behavesLike (Tuple Partial2 Verb123) Verb13 = true
behavesLike (Tuple Partial3 Verb3) Verb0 = true
behavesLike (Tuple Partial3 Verb23) Verb2 = true
behavesLike (Tuple Partial3 Verb123) Verb12 = true
behavesLike (Tuple Commencement vt) vt2 = vt == vt2
behavesLike (Tuple Reflection Verb12) Verb1 = true
behavesLike (Tuple Shift2 vt) vt2 = vt == vt2
behavesLike (Tuple Shift3 vt) vt2 = vt == vt2
behavesLike _ _ = false

-- TODO: Finish this list
behaviourOf :: WordType -> Inflection -> Maybe (Tuple WordType Boolean)
behaviourOf Verb12 Argument1 = Just (Tuple CountableNoun false)
behaviourOf Verb12 Argument2 = Just (Tuple CountableNoun false)
behaviourOf noun Possessive | canUsePI noun = Just (Tuple Verb1 false)
behaviourOf verb Perfect | isVerbType verb = Just (Tuple verb true)
behaviourOf wordtype Postfixed | canBePostfixed wordtype = Just (Tuple wordtype false)
behaviourOf _ _ = Nothing

usesPredicativeIdentity :: Inflection -> Maybe Inflection
usesPredicativeIdentity SpecificGerund = Just Gerund
usesPredicativeIdentity _ = Nothing

isVerbType :: WordType -> Boolean
isVerbType wordType = wordType `elem` [Verb0, Verb1, Verb2, Verb3, Verb12, Verb13, Verb23, Verb123]

canUsePI :: WordType -> Boolean
canUsePI =
   (_ `elem`
      [ CountableNoun
      , UncountableNoun
      , CountableAssociativeNoun
      , UncountableAssociativeNoun
      ])

canBePostfixed :: WordType -> Boolean
canBePostfixed =
   (_ `elem`
      [ Modifier
      , CountableAssociativeNoun
      , UncountableAssociativeNoun
      ])