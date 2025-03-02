{-# OPTIONS_GHC -Wno-missing-signatures #-}
module FactorAnalysis where

import Yaifl.Prelude

data Method = Stirred | Shaken | None deriving stock (Show, Eq, Ord, Generic)
data Serve = Rock | Crushed | StraightUp deriving stock (Show, Eq, Ord, Generic)
data Category = Sour | Negroni | Spritz | Tiki | SpiritForward | Beer | Highball deriving stock (Show, Eq, Ord, Generic)
data Colour = Orange | NoStrongColour | Yellow | Brown | DarkBrown | Red | Green deriving stock (Show, Eq, Ord, Generic)
data Origin = Classic | Modern | MidCentury | Late80s | Original deriving stock (Show, Eq, Ord, Generic)

data Drink = Drink
  { volume :: Double
  , numIngredients :: Int
  , strippedNumIngredients :: Int
  , name :: Text
  , method :: Method
  , category :: Category
  , origin :: Origin
  , colour :: Colour
  , serve :: Serve
  -- , attrs :: [Text]
  , ingredients :: [(Text, Double)]
  , ranking :: Int
  } deriving stock (Show, Eq, Ord, Generic)

lj = "lemon juice"
cog = "cognac"
gc = "green chartreuse"
ss = "simple syrup"
ang = "angostura bitters"
gi = "gin"
liq = ["liqueur"]
cit = ["citrus"]
bra = "brandy"
la = "long aged"
ua = "unaged"
bit = ["bitters"]
laR = "lightly aged rum"
grj = "grapefruit juice"
rag = "unaged rhum agricole"
mar = "maraschino"
opjr = "overproof unaged jamaican rum"
rum = "rum"
jam = "jamaican"
op = "overproof"

shakenCoupe n ca c o i r = mkDrink $ Drink 0 0 0 n Shaken ca o c StraightUp i r

stirredCoupe n ca c o i r = mkDrink $ Drink 0 0 0 n Stirred ca o c StraightUp i r

shakenRock :: Text -> Category -> Colour -> Origin -> [(Text, Double)] -> Int -> Drink
shakenRock n ca c o i r = mkDrink $ Drink 0 0 0 n Shaken ca o c Rock i r

negroni n ca c o i r = mkDrink $ Drink 0 0 0 n Stirred ca o c Rock i r

highball n c o i r = mkDrink $ Drink 0 0 0 n None Highball o c Rock i r

tiki n c o i r = mkDrink $ Drink 0 0 0 n Shaken Tiki o c Crushed i r

mkDrink d = d & #volume .~ ((case (method d) of
  Shaken -> (1.25 *)
  Stirred -> (1.17 *)
  None -> id) . foldl' (+) 0 . map snd $ ingredients d)

dash :: Int -> Double
dash = (0.3 *) . fromIntegral

attributes :: Map Text [Text]
attributes = fromList
  [ (lj, cit)
  , (cog, [bra, la])
  , (gc, liq)
  , (ang, bit)
  , (gi, [ua])
  , (grj, cit)
  , (laR, [rum, ua])
  , (rag, [rum, ua])
  , (opjr, [rum, ua, jam, op])
  ]

drinks :: [Drink]
drinks =
  [ shakenCoupe "Champs-Elysees" Sour Orange Classic [(lj, 20), (cog, 45), (gc, 10), (ss, 15), (ang, dash 1)] 1
  , shakenRock "Fitzgerald" Sour Yellow Late80s [(lj, 25), (gi, 50), (ss, 15), (ang, dash 2)] 1
  , shakenCoupe "Hemingway Daiquiri" Sour NoStrongColour MidCentury [(grj, 22), (laR, 2.25*30), (rag, 15), (opjr, 7.5), (mar, 15), (lj, 30), (ss, 7.5) ] 1
  , shakenRock "Amaretto Sour" Sour Brown Modern [] 1
  , shakenCoupe "English Marmalade" Sour Orange Modern [] 1
  , shakenCoupe "Wogan" Sour Orange Modern [] 1
  , negroni "Bitter Giuseppe" Negroni DarkBrown Modern [] 1
  , shakenCoupe "Blood and Sand" Sour Red Classic [] 1
  , shakenRock "Brasil" Sour Yellow Modern [] 1
  , (highball "La Cola Nostra" Brown Modern [] 1) { category = Spritz }
  , negroni "Boulevardier" Negroni Red Classic [] 1
  , negroni "Bijou" Negroni Red Classic [] 1
  , shakenCoupe "Monte Cassino" Sour Yellow Modern [] 1
  , tiki "Chartreuse Swizzle" Green Modern [] 1
  , tiki "20-70 Swizzle" DarkBrown Modern [] 1
  , tiki "Braulio Mai Tai" DarkBrown Original [] 1
  , stirredCoupe "Black Manhattan" SpiritForward DarkBrown Modern [] 1
  , shakenCoupe "Fort Lauderdale" Sour Orange Modern [] 1
  , shakenRock "Young Wolf" Sour NoStrongColour Original [] 1
  , (highball "Stout Fellow" DarkBrown Modern [] 1) { category = Beer, serve = StraightUp }
  , shakenRock "Whiskey Sour" Sour Brown Classic [] 1
  , negroni "Redbreast OF" SpiritForward Brown Classic [] 1
  , shakenCoupe "Pisco Sour" Sour NoStrongColour Classic [] 1
  , negroni "Corn 'N' Oil" SpiritForward DarkBrown Classic [] 1
  , highball "Italian Buck" Brown Modern [] 1
  , tiki "Artichoke Hold" Brown Modern [] 1
  , shakenRock "Paper Reindeer" Sour Brown Original [] 1
  , shakenCoupe "Too Soon" Sour Orange Modern [] 1
  , shakenCoupe "Trinidad Sour" Sour DarkBrown Modern [] 1
  ,

  ]