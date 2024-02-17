module Words where

import Solitude
import Database.SQLite.Simple
import qualified Data.Map as M
import qualified Data.Set as S
import System.Random.Stateful
import Network.HTTP.Req
import Control.Concurrent (threadDelay)
import Data.Aeson
import Control.Exception (catch)
import Eigen.SparseMatrix (SparseMatrix)
import qualified Eigen.SparseMatrix as E

iterateM_ :: Monad m => (a -> m a) -> a -> m b
iterateM_ f = g
    where g x = f x >>= g

data WordInfo = WordInfo
  { rank :: Int
  , plasticity :: Double
  , wordIsNew :: Bool
  , combos :: M.Map Text (Maybe Text)
  } deriving stock (Show, Generic)

type WordMap = M.Map Text WordInfo

data ComboRow = ComboRow Text Text (Maybe Text) Int Bool
  deriving stock (Show, Eq, Ord, Generic)

instance FromRow ComboRow where
  fromRow = ComboRow <$> field <*> field <*> field <*> field <*> field

runOneLoop :: (Int, WordMap) -> IO (Int, WordMap)
runOneLoop (cnt, m) = do
  (m', rows, attemptsTotal) <- runLoop m
  conn <- open "words.db"
  executeMany conn "INSERT INTO combos VALUES (?, ?, ?, ?, ?)" (map (\(ComboRow a b c d e) -> (a,b,fromMaybe "" c, show @Text d, if e then "1" :: Text else "0")) rows)
  execute conn "INSERT INTO stats VALUES (?, ?, ?, ?)" (("plasticity", (length rows) + cnt, length rows, (attemptsTotal)) :: (String, Int, Int, Int))
  close conn

  -- now we need to recalculate the plasticity for the elements we've adjusted
  let toAdjust = sortNub (mconcat $ map (\(ComboRow x y _ _ _) -> [x,y]) rows)
  putStrLn $ "Recalculating plasticities for " <> show (length toAdjust) <> " elements"
  pure $ (length rows + cnt, flipfoldl' (\a -> at a % _Just %~ recalculateInfo) m' toAdjust)

initMap :: IO (Int, WordMap)
initMap = do
  conn <- open "words.db"
  r <- query_ conn "SELECT * from combos" :: IO [ComboRow]
  print @Text $ "Found " <> show (length r) <> " existing combos"
  close conn
  -- start with the basic 4 elements, add the stuff from the db, and then recalculate the numbers
  pure $ (length r, M.map recalculateInfo $ foldl' (\mm cr -> view _2 $ addRow cr mm) seedMap r)

amountOptions :: Int
amountOptions = 10

tenPow :: Integer -> Double
tenPow n = 10.0 ^ n

roundN :: Double -> Integer -> Double
roundN x n = (fromIntegral @Integer . round)  (x * tenPow n) / tenPow n

getOneRandom :: Int -> WordMap -> IO (Text, WordInfo)
getOneRandom size m = do
  gen <- uniformRM (0, size-1) globalStdGen
  pure $ M.elemAt gen m

getOnePairingRandomly :: Int -> WordMap -> IO (Maybe ((Text, Text), Int))
getOnePairingRandomly size m = do
  (e1, p1) <- getOneRandom size m
  (e2, p2) <- getOneRandom size m
  rWeight <- uniformRM (0.0 :: Double, 1.0) globalStdGen
  let adding = e1 <> " (rank " <> show (rank p1) <> ") + " <> e2 <> " (rank " <> show (rank p2) <> ")"
      eRank = 1 + max (rank p1) (rank p2)
  case e2 `M.lookup` combos p1 of
    Just res -> do
      putStrLn $ toString $ "Skipping " <> adding <> " as it gives " <> fromMaybe "no result" res
      return Nothing
    Nothing ->
      if rWeight <= (plasticity p1 * plasticity p2)
      then do
        putStrLn $ toString $ "Doing " <> adding <> " -> ? (expected rank " <> show eRank <> ")"
        return $ Just ((e1, e2), eRank)
      else do
        putStrLn $ toString $ "Skipping " <> adding <> " because of the plasticities; (p=" <>
          show (roundN (plasticity p1) 2, roundN (plasticity p2) 2, roundN (plasticity p1 * plasticity p2) 2) <> ")"
        return Nothing

runLoop :: Map Text WordInfo -> IO (WordMap, [ComboRow], Int)
runLoop m = go m (M.size m) 0 0 []
  where
  go m' size numSoFar (attempts :: Int) newRows = do
    getOnePairingRandomly size m' >>= \case
      Nothing -> go m' size numSoFar (attempts+1) newRows
      -- r is the "expected" rank
      Just ((p1, p2), r) -> (do
        res <- doMatch m' p1 p2 r
        let (isNewToUs, mAfter, elemBefore) = addRow res m'
        when isNewToUs $ putStrLn "New to us!"
        whenJust elemBefore $ \newElem ->
          when (rank newElem > r) $ putStrLn $ "New rank: " <> show r <> ", down from " <> show (rank newElem)
        putStrLn "-----------------"
        if numSoFar >= amountOptions
        then do
          putStrLn $ "Done " <> show (numSoFar +1) <> " combos out of " <> show (attempts+1) <> " attempts. Saving to DB..."
          pure (mAfter, res : newRows, attempts+1)
        else go mAfter (if isNewToUs then size+1 else size) (numSoFar + 1) (attempts+1) (res : newRows))
        `catch` (\(SomeException _e) -> go m' size numSoFar attempts newRows)

data InfiniteResponse = IR
  { emoji :: Maybe Text
  , isNew :: Bool
  , result :: Text
  } deriving stock (Generic)
  deriving anyclass (FromJSON)

showResponse :: InfiniteResponse -> Text
showResponse ir = fromMaybe "" (emoji ir) <> " " <> result ir <> if isNew ir then " [New!]" else ""

doMatch :: WordMap -> Text -> Text -> Int -> IO ComboRow
doMatch wm x y r = do
  res <- runReq defaultHttpConfig $ do
    (bs :: JsonResponse InfiniteResponse) <- req GET (https "neal.fun" /: "api" /: "infinite-craft" /: "pair") NoReqBody jsonResponse $
      ("first" =: x) <>
      ("second" =: y) <>
      header "Referer" "https://neal.fun/infinite-craft/"
    let res = result (responseBody bs)
    let actualRank = rank <$> wm ^. at res
    putStrLn $ toString $ x <> " + " <> y <> " = " <> showResponse (responseBody bs) <> maybe "" (\acRank -> "(rank " <> show acRank <> ")") actualRank
    pure $ responseBody bs
  threadDelay 1000000
  pure $ ComboRow x y (if result res == "Nothing" then Nothing else Just $ result res) r (isNew res)

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

addRow :: ComboRow -> WordMap -> (Bool, WordMap, Maybe WordInfo)
addRow (ComboRow x y result rank isNew) wm =
  let wm' = wm & (at x %~ addComboRow y result rank isNew)
        & (at y %~ addComboRow x result rank isNew)
        -- and if it's a new to us element, we need to make a blank entry
        & maybe id (\r -> at r %~ (\case
            Nothing -> Just (WordInfo rank 1.0 isNew M.empty)
            Just r' -> Just r')) result
  in
  (maybe False (\x' -> not $ x' `M.member` wm) result
  , wm'
  , (\r -> wm ^? at r % _Just) =<< result
  )

addComboRow :: Text -> Maybe Text -> Int -> Bool -> Maybe WordInfo -> Maybe WordInfo
addComboRow otherElem result rank isNew Nothing = Just (WordInfo rank 1.0 isNew (M.singleton otherElem result))
addComboRow otherElem result newRank _isNew (Just wi) = Just (wi { rank = min (rank wi) newRank, combos = combos wi & at otherElem ?~ result })

makeIncidence :: KnownNat rows => KnownNat cols => Proxy (rows :: Nat) -> Proxy (cols :: Nat) -> WordMap -> (SparseMatrix rows cols Float, SparseMatrix rows cols Float)
makeIncidence _ _ wmOld = (E.fromList inDegreeMatrix, E.fromList outDegreeMatrix)
  where
    wm = M.map (\wi -> wi & #combos %~ M.map (\case
      Just "" -> Nothing
      x -> x)) wmOld
    mList :: [((Text, WordInfo), Int)]
    mList = zip (M.toList wm) [1..]
    nMap = M.fromList $ map (\((n, _wi), i) -> (n, i)) mList
    lookup' k = fromMaybe (error "") $ M.lookup k nMap
    comboMap = zip [0..] $ sortNub $ mconcat $ map (\((_n, wi), i) -> map (\(other, res) -> ((\case
        [x, y] -> (x, y)
        _ -> error "") $ sort [lookup' other, i], maybe 0 lookup' res)) (M.toList $ combos wi) ) mList
    (inDegreeMatrix, outDegreeMatrix) = partitionEithers $ mconcat $
      map (\(comboId, ((elem1, elem2), res)) -> [Left (elem1, comboId, 1), Left (elem2, comboId, 1), Right (res, comboId, 1)]) comboMap
      -- this travesty makes a list of combos, indexed by ints.
      -- maybe it should've just been done directly from the combo rows

makeSubgraph :: KnownNat rows => KnownNat cols => Proxy (rows :: Nat) -> Proxy (cols :: Nat) -> [(Int, Int, Int)] -> SparseMatrix rols cols Float
makeSubgraph = error ""

-- makeSubgraph (Proxy @6 (Proxy @2) [(0, 1, 2), (3, 4, 5)]

{-

-}