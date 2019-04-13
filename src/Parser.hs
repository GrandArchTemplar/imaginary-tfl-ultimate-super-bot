module Parser 
  ( (+..)
  , WordParser(..)
  , verbalizetf
  , verbalizeff
  , parseSword
  , isVerb
  , isPreposition
  )
  where

import Types 
  ( Letters (..)
  , Morpheme
  , PartOfSpeech (..)
  , Sword (..) 
  , Sverb (..)
  , Time (..)
  , Face (..)
  , Gender (..)
  , Count (..)
  , Conjugation (..)
  , emptyWord
  , rread
  , rshow
  , raw
  )

import Data.List ( isPrefixOf
                 , isSuffixOf
                 , find
                 , sortBy
                 ) 
import Data.Maybe (fromMaybe)

infixr 2 +..

(+..) :: Semigroup a => a -> Maybe a -> Maybe a
a  +.. Nothing = Just a
n +.. Just m = Just $ n <> m

infixr 2 ..+

(..+) :: Semigroup a => Maybe a -> a -> Maybe a
Nothing ..+ a = Just a
Just n ..+ m = Just $ n <> m 

newtype WordParser a b = WordParser { parse :: (a, b) -> (a, b) }

instance Semigroup (WordParser a b) where
  n <> m = WordParser { parse = parse n . parse m } 

instance Monoid (WordParser a b) where
  mappend = (<>)
  mempty  = WordParser { parse = id } 


parseP :: Morpheme -> [Letters] -> Maybe [Letters] 
parseP m w 
  | isPrefixOf m w = Just . drop (length m) $ w
  | otherwise = Nothing

parseS :: Morpheme -> [Letters] -> Maybe [Letters]
parseS m w 
  | isSuffixOf m w = Just . take (length w - length m) $ w
  | otherwise = Nothing

suffixParse :: Morpheme -> (Sword, [Letters]) -> (Sword, [Letters]) 
suffixParse m (w, l) = case parseS m l of
  Nothing -> (w, l)
  Just x  -> (w { suffix = m +.. suffix w }, x) 

prefixParse :: Morpheme -> (Sword, [Letters]) -> (Sword, [Letters]) 
prefixParse m (w, l) = case parseP m l of
  Nothing -> (w, l) 
  Just x  -> (w { prefix = prefix w ..+ m }, x)

flexionParse :: Morpheme -> (Sword, [Letters]) -> (Sword, [Letters])
flexionParse m (w, l) = case parseS m l of
  Nothing -> (w, l)
  Just x  -> (w { flexion = m +.. flexion w }, x)

postfixParse :: Morpheme -> (Sword, [Letters]) -> (Sword, [Letters]) 
postfixParse m (w, l) = case parseS m l of
  Nothing -> (w, l)
  Just x  -> (w { post = m +.. post w }, x)

rootParse :: (Sword, [Letters]) -> (Sword, [Letters])
rootParse (w, l) = (w { root = root w ++ l }, l) 

gPO :: [Morpheme] 
    -> (Morpheme -> (Sword, [Letters]) -> (Sword, [Letters])) 
    -> (Sword, [Letters]) -> (Bool, (Sword, [Letters]))
gPO ms p w = case find check good of
  Nothing -> (False, w)
  Just m  -> (True, p m w)
  where
    check :: Morpheme -> Bool
    check m = p m w /= w 
    good = sortBy (\x y -> compare (length y) (length x)) ms

gPI :: [Morpheme] 
    -> (Morpheme -> (Sword, [Letters]) -> (Sword, [Letters])) 
    -> (Sword, [Letters]) -> (Sword, [Letters])
gPI m p w = case gPO m p w of
  (False, w') -> w'
  (True, w'') -> gPI m p w''

gPS :: [Morpheme]
    -> (Morpheme -> (Sword, [Letters]) -> (Sword, [Letters]))
    -> (Sword, [Letters]) -> (Sword, [Letters])
gPS m p w = snd $ gPO m p w

_f :: String -> Morpheme
_f = map $ rread . pure 

_g :: [String] -> [Morpheme]
_g = map _f

postfixs :: [Morpheme]
postfixs = _g ["-то", "-либо", "-нибудь", "-ка", "ся", "сь"]
flexions :: [Morpheme]
flexions = _g 
  [ "а", "ы", "у", "е", "ам", "ах", "ом", "ами", "и", "ой", "о", "ю", "я", "мя"
  , "ой", "ый", "ая", "ое", "ые", "ий", "ье", "ья", "ие"
  , "ого", "его", "ьей", "ьего", "ых", "их", "ьих"
  , "ому", "ему", "ьему", "ей", "ьим", "ым", "им"
  , "ьими", "ыми", "ими"
  , "ешь", "ишь", "ит", "ут", "ют", "ят", "ат", "ем", "ете", "ите", "ет", ""
  ]
suffixs :: [Morpheme]
suffixs = _g 
  [ "а", "айщ", "анин", "ан", "аст", "ач", "ащ", "ар", "арь", "аг"
  , "б"
  , "в"
  , "е", "ев", "ева", "еват", "ее", "ей", "ейш", "ель", "ем", "ен", "ени", "енск", "енн", "есть", "ец", "ек", "еп"
  , "жд"
  , "знь"
  , "и", "ив", "ирова", "ива", "ик", "ин", "инн", "изм", "ист", "иц"
  , "к"
  , "л", "лив", "лк"
  , "м"
  , "н", "ни", "ник", "", "нича", "ну"
  , "ова", "оват", "ость", "онн", "о", "от", "овит"
  , "ск", "сн", "ств"
  , "тель", "ть"
  , "ун", "уч", "ущ", "учи"
  , "чат", "чив", "чик", "чиц"
  , "ш", "шн"
  , "щик", "щиц", "щин"
  , "юч", "ючи", "ющ"
  , "я", "як", "ян", "ян", "яч", "ящ"
  ]
prefixs :: [Morpheme]
prefixs = _g 
  [ "а", "анти", "ан", "аб", "абъ", "абс", "ад", "аг", "ак", "ал", "ам", "ап", "ар", "ас", "ат", "аф", "амби", "амб", "амбе", "анте", "архи", "апо", "авто"
  , "без", "бес", "благо"
  , "во", "въ", "в", "взаимно", "взаимо", "внутри", "все", "вы"
  , "гипер", "гипо", "голо", "гомо"
  , "до", "де", "диз", "дис", "диа", "диф", "дир", "ди"
  , "еже", "ев", "епи" 
  , "за"
  , "изо", "из", "ис", "ыз", "ыс", "ино", "испод", "ипо", "ин", "им", "ир", "ил", "интер", "интра", "инфра"
  , "кое-", "контр", "ко", "квази", "ком", "кол", "кор", "контра", "кросс"
  , "любо"
  , "между", "междо", "меж", "мимо", "мета"
  , "на", "наи", "надо", "над", "надъ", "не", "ни", "небезо", "небез", "небес", "недо", "низ", "нис", "низо"
  , "обезо", "обез", "обес", "обо", "объ", "о", "обще", "около", "от", "оф", "отъ", "омо", "об", "ок", "ог", "орфо", "орто"
  , "по", "па", "пере", "пре", "при", "подо", "пода", "под", "подъ", "поза", "после", "пра", "предо", "пред", "предъ", "переди", "про", "против", "противо", "пан", "пери", "пост"
  , "разо", "раз", "разъ", "рас", "роз", "рос", "ре", "ретро"
  , "сверх", "с", "среди", "со", "съ", "су", "сыз", "син", "сим", "суб", "суп", "супер", "супра"
  , "тре", "транс", "транз"
  , "у", "ультра"
  , "холо"
  , "циркум", "цис"
  , "через", "черес", "чрез", "чрес"
  , "эпи", "экс", "экз", "экстра", "эндо", "эм", "эн", "энтеро"
  ]
parseSuffix :: WordParser Sword [Letters]
parseSuffix = WordParser { parse = gPI suffixs suffixParse }
parsePrefix :: WordParser Sword [Letters]
parsePrefix = WordParser { parse = gPI prefixs prefixParse }
parseFlexion :: WordParser Sword [Letters]
parseFlexion = WordParser { parse = gPS flexions flexionParse }
parsePostfix :: WordParser Sword [Letters]
parsePostfix = WordParser { parse = gPI postfixs postfixParse } 
parseRoot :: WordParser Sword [Letters]
parseRoot = WordParser { parse = rootParse } 

testMorph :: [Morpheme]
testMorph = [[K, O], [K, O, N, T, R]]

testShit :: (Sword, [Letters])
testShit = (emptyWord, [K, O, N, T, R, K, O, P, I, D, O, R])

infixr 2 +.
(+.) :: Maybe a -> Maybe a -> Maybe a
Nothing +. x = x
x +. _ = x

_apart :: PartOfSpeech -> Sword -> Sword
_apart p s = Sword { root = root s, prefix = prefix s, suffix = suffix s, flexion = flexion s, post = post s, part = Just p } 

prepositions :: [Morpheme]  
prepositions  = _g 
  [ "в", "из-за", "из-под", "от", "к", "на", "по", "по-над", "над", "около", "под", "назад", "о", "об", "обо", "безо", "а-ля", "за", "ко", "кроме"
  ]

isPreposition :: Sword -> Sword
isPreposition s = case elem (raw s) prepositions of
  True -> _apart Preposition s
  False -> s

_vnf :: [Morpheme] 
_vnf = _g 
  ["ешь", "ишь", "ит", "ут", "ют", "ят", "ат", "ем", "ете", "ите", "у", "ю", "ет"]

_vpf :: [Morpheme]
_vpf = _g ["а", "о", "и", ""]

_vps :: [Letters]
_vps = [A, I, E, Jo, O, U, Y, Ja, Ae, Ju]

isVerb :: Sword -> Sword
isVerb s = case elem (fromMaybe [] $ flexion s) _vnf of
  True  -> _apart Verb s 
  False -> case fromMaybe [] $ suffix s of
    m@(x:y:xs) -> case last m of
      L -> case (elem (fromMaybe [] $ flexion s) _vpf) of
        False -> s
        True  -> case elem (last (init m)) _vps of 
          True -> _apart Verb s
          _    -> s
      _ -> s
    _ -> s

verbalizeff :: Sword -> Sverb
verbalizeff s = let 
  v = isVerb s
  f = fromMaybe [] $ flexion s
    in case part v of 
  Just Verb -> let t = case elem f _vpf of
                     True  -> Past
                     False -> Present 
      in case t of 
          Past    -> case f of
            [I] -> Sverb { sword = s, time = t, count = Many, secondary = Right Nothing }
            [O] -> Sverb { sword = s, time = t, count = Solo, secondary = Right (Just Mid) }
            [A] -> Sverb { sword = s, time = t, count = Solo, secondary = Right (Just Female) }
            []  -> Sverb { sword = s, time = t, count = Solo, secondary = Right (Just Male) }
            _   -> error "Окончание не прошедшего времени"
          Present -> case f of 
            [U]           -> Sverb { sword = s, time = t, count = Solo, secondary = Left (First, Fst) }
            [Ju]          -> Sverb { sword = s, time = t, count = Solo, secondary = Left (First, Snd) }
            [E, M]        -> Sverb { sword = s, time = t, count = Many, secondary = Left (First, Fst) }
            [I, M]        -> Sverb { sword = s, time = t, count = Many, secondary = Left (First, Snd) }
            [E, Sh, Soft] -> Sverb { sword = s, time = t, count = Solo, secondary = Left (Second, Fst) }
            [I, Sh, Soft] -> Sverb { sword = s, time = t, count = Solo, secondary = Left (Second, Snd) }
            [E, T, E]     -> Sverb { sword = s, time = t, count = Many, secondary = Left (Second, Fst) }
            [I, T, E]     -> Sverb { sword = s, time = t, count = Many, secondary = Left (Second, Snd) }
            [E, T]        -> Sverb { sword = s, time = t, count = Solo, secondary = Left (Third, Fst) }
            [I, T]        -> Sverb { sword = s, time = t, count = Solo, secondary = Left (Third, Snd) }
            [U, T]        -> Sverb { sword = s, time = t, count = Many, secondary = Left (Third, Fst) }
            [Ju, T]       -> Sverb { sword = s, time = t, count = Many, secondary = Left (Third, Fst) }
            [A, T]        -> Sverb { sword = s, time = t, count = Many, secondary = Left (Third, Snd) }
            [Ja, T]       -> Sverb { sword = s, time = t, count = Many, secondary = Left (Third, Snd) }
  _        -> error "Не глагол"

verbalizetf :: Sverb -> Sverb
verbalizetf sv = let
  s = sword sv
  s'= case secondary sv of
        Right _ -> fmap (++ [L]) $ suffix s 
        Left _  -> fmap init $ suffix s
  f = case (count sv, secondary sv) of
        (Solo, Left (First, Fst))   -> [U]
        (Solo, Left (First, Snd))   -> [Ju]
        (Many, Left (First, Fst))   -> [E, M]
        (Many, Left (First, Snd))   -> [I, M]
        (Solo, Left (Second, Fst))  -> [E, Sh, Soft]
        (Solo, Left (Second, Snd))  -> [I, Sh, Soft]
        (Many, Left (Second, Fst))  -> [E, T, E]
        (Many, Left (Second, Snd))  -> [I, T, E]
        (Solo, Left (Third, Fst))   -> [E, T]
        (Solo, Left (Third, Snd))   -> [I, T]
        (Many, Left (Third, Fst))   -> [U, T]
        (Many, Left (Third, Snd))   -> [A, T]
        (Many, Right Nothing)       -> [I]
        (Solo, Right (Just Mid))    -> [O]
        (Solo, Right (Just Female)) -> [A]
        (Solo, Right (Just Male))   -> []
        _                           -> error "Невозможное сочетание свойств"
          in sv { sword = s { flexion = Just f, suffix = s' } } 
      


parseSword :: String -> Sword
parseSword s = fst . parse (parseRoot <> parsePrefix <> parseSuffix <> parseFlexion <> parsePostfix) $ (emptyWord, map (rread . pure) s)


