module Analizer (analize) where

import Types 
  ( Sword (..)
  , Letters (..)
  , Sverb (..)
  , PartOfSpeech(..)
  , Gender(..)
  , Count (..)
  , Face (..)
  , Conjugation (..)
  , raw
  , rshow
  )

import Parser 
  ( parseSword
  , verbalizeff
  , verbalizetf
  , isVerb
  , isPreposition
  )

import Data.List 
  ( find
  , intercalate 
  )


crutch :: Sverb -> [Sword]
crutch sv = let 
  isv = verbalizetf (sv { count = Solo, secondary = Left (First, Snd)} )
  psv = verbalizetf (sv { count = Solo, secondary = Right (Just Male) } )
  s = sword isv
    in case (count sv, secondary sv) of
        (Solo, Left (First, _))     -> map parseSword ["я", "тоже"] ++ [sword isv]
        (Many, Left (First, _))     -> map parseSword ["а", "я", "не"] ++ [sword isv]
        (Solo, Left (Second, _))    -> map parseSword ["помню", "когда", "я"] ++ [sword psv]
        (Many, Left (Second, _))    -> map parseSword ["многие", "из", "нас", "видели", "почему", "я"] ++ [sword psv] ++ map parseSword ["ежедневно"]
        (Solo, Left (Third, _))     -> map parseSword ["дай", "мне", "время", "и", "я"] ++ [sword psv]
        (Many, Left (Third, _))     -> map parseSword ["они", "могли", "бы", "не"] ++ [s { flexion = Just [], suffix = fmap (++ [T, Soft]) $ suffix s }]
        (Many, Right Nothing)       -> map parseSword ["это", "потому", "что", "они", "не", "являлись", "избранными", "судьбой"]
        (Solo, Right (Just Mid))    -> map parseSword ["и", "мне", "это", "нравилось"]
        (Solo, Right (Just Female)) -> map parseSword ["а", "вот", "если", "бы", "она", "была", "им"]
        (Solo, Right (Just Male))   -> map parseSword ["он", "такой", "молодец"]

analize :: String -> String
analize s = let sws = map parseSword $ words (filter (\x -> elem x "абвгдеёжзийклмнопрстуфхцчшщъыьэюя- ") s) in 
 intercalate " " $ map (rshow . raw) (answer sws) where
  answer :: [Sword] -> [Sword]
  answer ss = let 
     iss = (map (isVerb . isPreposition)) ss
     verbIs sw = case part sw of
      Just Verb -> True
      _         -> False
      in case find verbIs iss of
        Just v -> crutch (verbalizeff v)
        Nothing -> map parseSword $ words "мне тяжело распознавать вашу речь к моему сожалению видите ли я хлебушек"
