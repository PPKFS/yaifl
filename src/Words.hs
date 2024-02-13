module Words where

import Solitude
import Database.SQLite.Simple
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Exception (bracket_)

data WordInfo = WordInfo
  { rank :: Int
  , plasticity :: Double
  , isNew :: Bool
  , combos :: M.Map Text (Maybe Text)
  } deriving stock (Show, Generic)

type WordMap = M.Map Text WordInfo

data ComboRow = ComboRow Text Text (Maybe Text) Int Bool
  deriving stock (Show, Eq, Ord)

instance FromRow ComboRow where
  fromRow = ComboRow <$> field <*> field <*> field <*> field <*> field

initMap :: IO WordMap
initMap = do
  conn <- open "words.db"
  r <- query_ conn "SELECT * from combos" :: IO [ComboRow]
  let m = flipfoldl' addRow seedMap r
      m' = M.map recalculateInfo m
  close conn
  pure m'

amountOptions :: Int
amountOptions = 100

runLoop :: Map Text WordInfo -> IO WordMap
runLoop m = do
  let prio = take amountOptions $ getNextSteps m

recalculateInfo :: WordInfo -> WordInfo
recalculateInfo wi = wi & #plasticity .~ (let s = M.size (combos wi) in
  if s == 0 then 1.0 else fromIntegral (S.size (S.fromList (M.elems (combos wi)))) / fromIntegral s)

seedMap :: Map Text WordInfo
seedMap = M.fromList
  [ initElem "Water"
  , initElem "Earth"
  , initElem "Wind"
  , initElem "Fire"
  ]
  where
    initElem :: Text -> (Text, WordInfo)
    initElem = (, WordInfo 0 1 False M.empty)

addRow :: ComboRow -> WordMap -> WordMap
addRow (ComboRow x y result rank isNew) = (at x %~ addComboRow y result rank isNew) . (at y %~ addComboRow x result rank isNew)

addComboRow :: Text -> Maybe Text -> Int -> Bool -> Maybe WordInfo -> Maybe WordInfo
addComboRow otherElem result rank isNew Nothing = Just (WordInfo rank 1.0 isNew (M.singleton otherElem result))
addComboRow otherElem result newRank _isNew (Just wi) = Just (wi { rank = min (rank wi) newRank, combos = combos wi & at otherElem ?~ result })
