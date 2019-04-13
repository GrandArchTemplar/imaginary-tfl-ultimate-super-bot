{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Types 
  ( Letters(..)
  , Morpheme(..)
  , PartOfSpeech(..)
  , Sword(..)
  , Sverb(..)
  , Time(..)
  , Count(..)
  , Face(..)
  , Gender(..)
  , Conjugation(..)
  , emptyWord
  , emptyVerb
  , rread 
  , rshow
  , raw
  ) 
  where

import Data.List (intercalate) 

data Letters = A    | B   | V 
             | G    | D   | E 
             | Jo   | Je  | Z 
             | I    | J   | K 
             | L    | M   | N 
             | O    | P   | R 
             | S    | T   | U 
             | F    | H   | Ts 
             | Tch  | Sh  | Tsch 
             | Hard | Y   | Soft 
             | Ae   | Ju  | Ja 
             | Def
             deriving (Eq, Show)

type Morpheme = [Letters]

class RussianRead x where
  rread :: String -> x

instance RussianRead Letters where
  rread x = case x of
    "а" -> A
    "б" -> B
    "в" -> V
    "г" -> G
    "д" -> D
    "е" -> E
    "ё" -> Jo
    "ж" -> Je
    "з" -> Z
    "и" -> I 
    "й" -> J
    "к" -> K
    "л" -> L
    "м" -> M 
    "н" -> N
    "о" -> O
    "п" -> P
    "р" -> R
    "с" -> S
    "т" -> T
    "у" -> U
    "ф" -> F
    "х" -> H
    "ц" -> Ts
    "ч" -> Tch
    "ш" -> Sh
    "щ" -> Tsch
    "ъ" -> Hard
    "ы" -> Y
    "ь" -> Soft
    "э" -> Ae
    "ю" -> Ju
    "я" -> Ja
    "-" -> Def
    _   -> error "Not exhaustive letter"

class RussianShow x where
  rshow :: x -> String

instance RussianShow Letters where
  rshow x = case x of
    A     -> "а"
    B     -> "б"
    V     -> "в"
    G     -> "г" 
    D     -> "д"
    E     -> "е" 
    Jo    -> "ё" 
    Je    -> "ж" 
    Z     -> "з" 
    I     -> "и" 
    J     -> "й"
    K     -> "к" 
    L     -> "л"
    M     -> "м" 
    N     -> "н" 
    O     -> "о"
    P     -> "п"
    R     -> "р" 
    S     -> "с"
    T     -> "т"
    U     -> "у"
    F     -> "ф"
    H     -> "х"
    Ts    -> "ц"
    Tch   -> "ч"
    Sh    -> "ш"
    Tsch  -> "щ"
    Hard  -> "ъ"
    Y     -> "ы"
    Soft  -> "ь"
    Ae    -> "э"
    Ju    -> "ю"
    Ja    -> "я"
    Def   -> "-"
instance RussianShow Morpheme where
  rshow = concatMap rshow 

data PartOfSpeech = Noun 
  | Verb 
  | Adjective 
  | Adverb 
  | Pronoun 
  | Union 
  | Preposition 
  | Particle 
  | Interjection
  | Numeral
  deriving Eq

instance RussianShow PartOfSpeech where
  rshow Verb          = "глагол"
  rshow Adjective     = "прилагательное"
  rshow Noun          = "существительное"
  rshow Adverb        = "наречие"
  rshow Union         = "союз"
  rshow Preposition   = "предлог"
  rshow Interjection  = "междометие"
  rshow Numeral       = "числительное"
  rshow Particle      = "частица"
  rshow Pronoun       = "местоимение"

data Sword = Sword {
 root :: Morpheme,
 prefix :: Maybe Morpheme,
 suffix :: Maybe Morpheme,
 flexion :: Maybe Morpheme,
 post :: Maybe Morpheme,
 part :: Maybe PartOfSpeech
} deriving Eq 


instance RussianShow x => RussianShow (Maybe x) where
  rshow = maybe "" rshow  


instance RussianShow Sword where
  rshow Sword { root = r, prefix = p, suffix = s, flexion = f, part = t, post = o } = '-' : rshow p ++ '|' : '(' : rshow r ++ ')' : '/' : rshow s ++ '\\' : helper f t ++ '/':'/': rshow o ++ "\\\\" where
    helper :: Maybe Morpheme -> Maybe PartOfSpeech -> String
    helper x Nothing  = '[' : rshow x ++ "]"
    helper x (Just m) = case m of
      Union -> rshow x
      Preposition -> rshow x
      Particle -> rshow x 
      Interjection -> rshow x
      Adverb -> rshow x
      _ -> '[' : rshow x ++ "]"

data Time = Past | Present | Future deriving Show

data Count = Solo | Many deriving Show 

data Face = First | Second | Third deriving Show

data Gender = Male | Female | Mid deriving Show 

data Conjugation = Fst | Snd deriving Show 

data Sverb = Sverb
  { sword :: Sword
  , time :: Time
  , count :: Count
  , secondary :: Either (Face, Conjugation) (Maybe Gender)
  }

instance RussianShow Sverb where
  rshow sv = intercalate " " (rshow (sword sv) : ([show . time, show . count, show . secondary] <*> [sv]))

emptyWord :: Sword 
emptyWord = Sword { root = [], prefix = Nothing, suffix = Nothing, flexion = Nothing, part = Nothing, post = Nothing } 

emptyVerb :: Sverb 
emptyVerb = Sverb { sword = emptyWord, time = Present, count = Solo, secondary = Left (Second, Fst) } 

raw :: Sword -> [Letters]
raw Sword { root = r, prefix = p, suffix = s, flexion = f, part = _, post = o } = _f $ rshow p ++ rshow r ++ rshow s ++ rshow f ++ rshow o where
  _f = map $ rread . pure
