module Words where

import Solitude hiding (get, modify, State)
import Database.SQLite.Simple
import qualified Data.Map as M
import qualified Data.Set as S
import System.Random.Stateful
import Network.HTTP.Req
import Control.Concurrent (threadDelay)
import Data.Aeson
import Control.Exception (catch)
import qualified GHC.List as L
import qualified Data.List as L
import Effectful.State.Static.Local
iterateM_ :: Monad m => (a -> m a) -> a -> m b
iterateM_ f = g
    where g x = f x >>= g

data WordInfo' a = WordInfo
  { rank :: Int
  , plasticity :: Double
  , wordIsNew :: Bool
  , combos :: M.Map a (Maybe a)
  , madeBy :: S.Set (a, a)
  } deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type WordInfo = WordInfo' Text
type WordMap' a = M.Map a (WordInfo' a)
type WordMap = WordMap' Text

data ComboRow = ComboRow Text Text (Maybe Text) Int Bool
  deriving stock (Show, Eq, Ord, Generic)

instance FromRow ComboRow where
  fromRow = ComboRow <$> field <*> field <*> field <*> field <*> field

lookup' :: Ord k => Map k a -> k -> a
lookup' m k = fromMaybe (error "") $ M.lookup k m

backFillCombos :: WordMap -> IO ([Int], WordMap)
backFillCombos wmOuter = runEff $ runState wmOuter $ do
  forM_ (M.toList wmOuter) $ \(k, v) -> do
    let matches = map (\(k', c) -> if k <= k' then ((k, k'), c) else ((k', k), c)) (M.toList $ combos v)
    mapM_ (\((kThis, k'), c) -> modify (\wm' -> wm' & at (fromMaybe "Nothing" c) % _Just % #madeBy %~ S.insert (kThis, k'))) matches
  -- so now we want, for each element, to take the smallest rank from all of its made-by
  let iterIt :: IOE :> es => State WordMap :> es => [Int] -> Eff es [Int]
      iterIt r = do
        wm <- get
        changes <- forM (M.toList wm) $ \(k, v) -> do
          let possRank = foldr min 1000000 $ flip map (S.toList $ madeBy v) $ \(k1, k2) -> 1+max (rank $ lookup' wm k1) (rank $ lookup' wm k2)
          if possRank < (rank v)
          then do
            print $ k <> " going from " <> show (rank v) <> " to " <> show (possRank)
            modify (\wm' -> wm' & at k % _Just % #rank .~ possRank)
            pure (1 :: Int)
          else
            pure 0
        let c = sum changes
        print $ "Done " <> show c
        print "-----------"
        if c == 0 then pure (0:r) else iterIt (c:r)
  iterIt []
runOneLoop :: Int -> (Int, WordMap) -> IO (Int, WordMap)
runOneLoop rankNum (cnt, m) = do
  (m', rows, attemptsTotal) <- runLoop rankNum m
  conn <- open "words.db"
  executeMany conn "INSERT INTO combos VALUES (?, ?, ?, ?, ?)" (map (\(ComboRow a b c d e) -> (a,b,fromMaybe "" c, show @Text d, if e then "1" :: Text else "0")) rows)
  execute conn "INSERT INTO stats VALUES (?, ?, ?, ?)" (("plasticity", length rows + cnt, length rows, attemptsTotal) :: (String, Int, Int, Int))
  close conn

  -- now we need to recalculate the plasticity for the elements we've adjusted
  let toAdjust = sortNub (mconcat $ map (\(ComboRow x y z _ _) -> [x,y] <> maybeToList z) rows)
  putStrLn $ "Recalculating plasticities for " <> show (length toAdjust) <> " elements"
  let m2 = flipfoldl' (\a -> at a % _Just %~ recalculateInfo) m' toAdjust
  pure (length rows + cnt, m2 )

initMap :: IO (Int, WordMap)
initMap = do
  conn <- open "words.db"
  r <- query_ conn "SELECT * from combos" :: IO [ComboRow]
  print @Text $ "Found " <> show (length r) <> " existing combos"
  close conn
  -- start with the basic 4 elements, add the stuff from the db, and then recalculate the numbers
  pure (length r, M.map recalculateInfo $ foldl' (\mm cr -> view _2 $ addRow cr mm) seedMap r)

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
      getOnePairingRandomly size m--return Nothing
    Nothing ->
      if rWeight <= plasticity p1 * plasticity p2
      then do
        putStrLn $ toString $ "Doing " <> adding <> " -> ? (expected rank " <> show eRank <> ")"
        return $ Just ((e1, e2), eRank)
      else do
        --utStrLn $ toString $ "Skipping " <> adding <> " because of the plasticities; (p=" <>
        --  show (roundN (plasticity p1) 2, roundN (plasticity p2) 2, roundN (plasticity p1 * plasticity p2) 2) <> ")"
        getOnePairingRandomly size m

runLoop :: Int -> Map Text WordInfo -> IO (WordMap, [ComboRow], Int)
runLoop rankThreshold m = go m (M.size m) 0 0 []
  where
  go m' size numSoFar (attempts :: Int) newRows = do
    let m2 = M.filter (\a -> rank a <= rankThreshold) m
        size2 = M.size m2
    getOnePairingRandomly size2 m2 >>= \case
      Nothing -> go m' size numSoFar (attempts+1) newRows
      -- r is the "expected" rank
      Just ((p1, p2), r) -> (do
        res <- doMatch m' p1 p2 r
        let (isNewToUs, mAfter, elemBefore) = addRow res m'
        when isNewToUs $ putStrLn "New to us!"
        case elemBefore of
          Nothing -> pure mAfter
          Just oldElem -> if
            rank oldElem > r
            then do
              putStrLn $ "NEW RANK POG: " <> show r <> ", down from " <> show (rank oldElem)
              pure mAfter
            else
              pure mAfter

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
showResponse ir = fromMaybe "" (emoji ir) <> " " <> result ir <> if isNew ir then " [NEW! NEW! NEW! NEW!]" else ""

doMatch :: WordMap -> Text -> Text -> Int -> IO ComboRow
doMatch wm x y r = do
  res <- runReq defaultHttpConfig $ do
    (bs :: JsonResponse InfiniteResponse) <- req GET (https "neal.fun" /: "api" /: "infinite-craft" /: "pair") NoReqBody jsonResponse $
      ("first" =: x) <>
      ("second" =: y) <>
      header "Referer" "https://neal.fun/infinite-craft/" <>
      header "Host" "neal.fun" <>
      header "User-Agent" "Mozilla/5.0 (X11; Linux x86_64; rv:121.0) Gecko/20100101 Firefox/121.0"
    let res = result (responseBody bs)
    let actualRank = rank <$> wm ^. at res
    putStrLn $ toString $ x <> " + " <> y <> " = " <> showResponse (responseBody bs) <> maybe "" (\acRank -> "(rank " <> show acRank <> ")") actualRank
    pure $ responseBody bs
  threadDelay 1500000
  pure $ ComboRow x y (if result res == "Nothing" then Nothing else Just $ result res) r (isNew res)

rankWeight :: Double
rankWeight = 10.0
recalculateInfo :: WordInfo -> WordInfo
recalculateInfo wi = wi & #plasticity .~ (let s = M.size (combos wi) in
  (if s == 0 then 1.0 else fromIntegral (S.size (S.fromList (M.elems (combos wi)))) / fromIntegral s) / (fromIntegral (rank wi) / rankWeight)  )

seedMap :: Map Text WordInfo
seedMap = M.fromList
  [ initElem "Water"
  , initElem "Earth"
  , initElem "Wind"
  , initElem "Fire"
  ]
  where
    initElem :: Text -> (Text, WordInfo)
    initElem = (, WordInfo 0 1 False M.empty S.empty)

addRow :: ComboRow -> WordMap -> (Bool, WordMap, Maybe WordInfo)
addRow (ComboRow x y result rank isNew) wm =
  let wm' = wm & at x %~ addComboRow y result rank isNew
        & at y %~ addComboRow x result rank isNew
        -- and if it's a new to us element, we need to make a blank entry
        & maybe id (\r -> at r %~ (\case
            Nothing -> Just (WordInfo rank 1.0 isNew M.empty S.empty)
            Just r' -> Just r')) result
  in
  (maybe False (\x' -> not $ x' `M.member` wm) result
  , wm'
  , (\r -> wm ^? at r % _Just) =<< result
  )

addComboRow :: Text -> Maybe Text -> Int -> Bool -> Maybe WordInfo -> Maybe WordInfo
addComboRow otherElem result rank isNew Nothing = Just (WordInfo rank 1.0 isNew (one (otherElem, result)) S.empty)
addComboRow otherElem result newRank _isNew (Just wi) = Just (wi { rank = min (rank wi) newRank, combos = combos wi & at otherElem ?~ result })

wordMapToInt :: WordMap -> ([(Int, Int)], [(Int, Int)], Map Text Int, Map Int Text, WordMap' Int)
wordMapToInt wm = (news, windowed, nMap, revNMap, w'')
  where
    amendNothing = ("Nothing", WordInfo 0 0 False M.empty S.empty)
    initialMList = amendNothing : sortBy (\(_, wi) (_, wi2) -> rank wi `compare` rank wi2) (M.toList wm)
    mList :: [((Text, WordInfo), Int)]
    mList = zip initialMList [0..]
    grps = L.groupBy (\((_, wi1), _) ((_, wi2), _) -> rank wi1 == rank wi2) mList
    groupedRanks = zip [(0 :: Int)..] $ map (snd . L.head) grps
    windowed = (0, 1) : zipWith (curry (\((_, a), (i, b)) -> (i, if i == 1 then 4 else b-a))) groupedRanks (L.tail groupedRanks)
    news = (0, 1) : zip [(1 :: Int)..] (map (length . filter (\(a, _) -> wordIsNew (snd a))) grps)

    nMap = M.fromList $ map (\((n, _wi), i) -> (n, i)) mList
    revNMap = M.fromList $ map (\((n, _wi), i) -> (i, n)) mList
    lookup' k = fromMaybe (error "") $ M.lookup k nMap
    w = M.mapKeys lookup' wm
    w'' = M.map (\wi -> WordInfo
      { rank = rank wi
      , plasticity = plasticity wi
      , wordIsNew = wordIsNew wi
      , combos = M.map (fmap lookup') $ M.mapKeys lookup' (combos wi)
      , madeBy = S.empty
      }) w

tidyUpMap :: WordMap -> WordMap
tidyUpMap = M.delete "" . M.map (\wi -> wi & #combos %~ M.map (\case
      Just "" -> Nothing
      x -> x))

data EndingReason a = HN a | NR a | LO a
  deriving stock (Eq, Ord, Show)
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

randomWalk :: Show a => Ord a => a -> Int -> WordMap' a -> IO [(EndingReason a, NonEmpty a)]
randomWalk pointed mx wm = do
  seedElem <- randomRM (1, mx-1) globalStdGen
  let foundElem = M.keys wm L.!! seedElem
  (er, res, alts) <- (\(er, (res1, alt1)) -> (er, reverse res1, reverse alt1)) <$> go foundElem (foundElem, [foundElem], [])
  let mkSublists _ [] = []
      mkSublists (m:ms) (o:os) = [(er, m:|ms), (er, o:|ms)] <> mkSublists ms os
      mkSublists _ _ = error ""
  pure (mkSublists res alts)
  where
    lookup' m k = fromMaybe (error "") $ M.lookup k m
    -- we are at element elemI, and we've so far seen s (by combining with s2)
    go elemI (k, s, s2) = do
      let elem' = lookup' wm elemI
      let options = M.keys $ combos elem'
      --putStrLn $ "Element: " <> show elemI
      case options of
        -- elem goes nowhere, so we just want to return the combos so far
        [] -> do
          --putStrLn "Goes nowhere"
          pure (NR elemI, (s, s2))
        xs -> do
          i <- randomRM (0, length xs-1) globalStdGen
          let other = xs L.!! i
          let pick = lookup' (combos elem') other
          --putStrLn $ "Chosen: " <> show other
          --putStrLn $ "Goes to: " <> show pick
          case pick of
            Nothing -> pure (HN elemI, (pointed : s, other : s2))
            Just r
              | r == k || r `elem` s || r `elem` s2 || r == other -> pure (LO r, (r : s, other : s2))
            Just r -> go r (k, r : s, other : s2)

data Results = Results
  { toIdMap :: Map Text Int
  , fromIdMap :: Map Int Text
  , rawMap :: WordMap
  , walks :: Map (Int, Int) Int
  , perRanks :: [(Int, Int)]
  , newPer :: [(Int, Int)]
  , chainEndReasons :: Map (EndingReason Int) Int
  } deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
doNGoes :: Bool -> Int -> Int -> IO Results
doNGoes doSubchains noteEvery n = do
  m <- tidyUpMap . snd <$> initMap
  let (news, groupedRanks, nMap, revNMap, wm) = wordMapToInt m
      totSize = M.size wm
      -- seedMatrix = makeEmptySparse ((\(SomeNat p) -> p) (someNatVal $ fromIntegral totSize))
  traces <- mconcat <$$> forM [0..n] $ \i -> do
    when (i `mod` noteEvery == 0) $ putStrLn $ "Done " <> show i
    res <- randomWalk 0 totSize wm
    if doSubchains then pure res else pure $ maybeToList (listToMaybe res)
  --putStrLn $ "Traces: " <> show traces
  let l = mconcat (map (\(_, rIndex :| rst) -> let rs = ordNub rst in map (\r -> ((rIndex,r), 1)) rs) traces)
      endRes = map (\(er, _) ->(er, 1)) traces
  putStrLn $ "Total elements: " <> show totSize
  putStrLn $ "Ranks: " <> show groupedRanks
  putStrLn $ "new per rank: " <> show news
  pure $ Results nMap revNMap m (M.fromListWith (+) l) groupedRanks news (M.fromListWith (+) endRes)

mapEndings :: Results -> Map (EndingReason Text) Int
mapEndings r = let lookup' k = fromMaybe (error "") $ k `M.lookup` (fromIdMap r) in
  M.mapKeys (\case
  HN a -> HN (lookup' a)
  LO a -> LO (lookup' a)
  NR a -> NR (lookup' a)
  ) $ chainEndReasons r
