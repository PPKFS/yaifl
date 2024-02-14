module Words where

import Solitude
import Database.SQLite.Simple
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Exception (bracket_)
import System.Random.Stateful

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
  let m = foldl' (\m cr -> snd $ addRow cr m) seedMap r
      m' = M.map recalculateInfo m
  close conn
  pure m'

amountOptions :: Int
amountOptions = 100

getOneRandom :: Int -> WordMap -> IO (Text, WordInfo)
getOneRandom size m = do
  gen <- uniformRM (0, size-1) globalStdGen
  pure $ M.elemAt gen m

getOnePairingRandomly :: Int -> WordMap -> IO (Maybe (Text, Text))
getOnePairingRandomly size m = do
  (e1, p1) <- getOneRandom size m
  (e2, p2) <- getOneRandom size m
  pure $ if e2 `M.member` (combos p1) then Nothing else Just (e1, e2)

runLoop :: Map Text WordInfo -> IO WordMap
runLoop m = do
  let mSize = M.size m
  fst <$> go m mSize 0 []
  where
  go m' size numSoFar newRows = do
    prio <- getOnePairingRandomly size m'
    case prio of
      Nothing -> go m' size numSoFar newRows
      Just (p1, p2) -> do
        res <- doMatch p1 p2
        let (isNewToUs, mAfter) = addRow res m'
        if numSoFar >= amountOptions
        then do
          print mAfter
          pure (mAfter, (res : newRows))
        else go mAfter (if isNewToUs then size+1 else size) (numSoFar + 1) (res : newRows)

doMatch x y = pure $ ComboRow x y (Just $ x <> y) 9 False
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

addRow :: ComboRow -> WordMap -> (Bool, WordMap)
addRow (ComboRow x y result rank isNew) wm =
  (maybe False (\x' -> not $ x' `M.member` wm) result, wm & (at x %~ addComboRow y result rank isNew) & (at y %~ addComboRow x result rank isNew))

addComboRow :: Text -> Maybe Text -> Int -> Bool -> Maybe WordInfo -> Maybe WordInfo
addComboRow otherElem result rank isNew Nothing = Just (WordInfo rank 1.0 isNew (M.singleton otherElem result))
addComboRow otherElem result newRank _isNew (Just wi) = Just (wi { rank = min (rank wi) newRank, combos = combos wi & at otherElem ?~ result })
