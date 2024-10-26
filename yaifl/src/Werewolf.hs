{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE RecordWildCards #-}
module Werewolf where

import Yaifl.Prelude hiding (modify, State)
import qualified Data.Set as S
import qualified Data.Map as M
import Effectful.State.Dynamic
import qualified Data.Text.Lazy.Builder as TB
import System.Random.Shuffle
import Data.Char (toUpper)
import qualified Data.Text as T
import Control.Monad.Random.Class
import Data.List (delete)

data Role =
  Villager
  | Werewolf
  | Troublemaker
  | Seer
  | Robber
  deriving stock (Eq, Ord, Show, Generic)

names :: [Text]
names = map (over _head toUpper) [
  "bernard",
  "kathleen",
  "kevin",
  "xena",
  "bradley",
  "dorothy",
  "ian",
  "hilary",
  "wayne",
  "geraldine",
  "matthew",
  "laura",
  "aaron",
  "cindy",
  "jonathan",
  "cindy",
  "simeon",
  "sharon",
  "peter",
  "cathy",
  "harvey",
  "jodi",
  "gordon",
  "hilary",
  "liam",
  "xaria",
  "zane",
  "cathy",
  "jacob",
  "noreen",
  "clifford",
  "eleanor",
  "quentin",
  "hayley",
  "hugh",
  "diana",
  "kevin",
  "ruth",
  "clifford",
  "kathleen",
  "hugh",
  "lena",
  "brian",
  "marilyn",
  "timothy",
  "hayley",
  "barry",
  "chelsea",
  "ronald",
  "katie"]

type Prop = (Int, Fact)
type Knowledge = S.Set Prop
type CentreCardL = Lens' (Role, Role, Role) Role

centreL :: Int -> CentreCardL
centreL = \case
    1 -> _1
    2 -> _2
    3 -> _3
    _ -> error "impossible"

data Fact = AgentIsRole Text Role | CentreCardIsRole Int Role | AgentBelieves Text Prop
  deriving stock (Eq, Ord, Show, Generic)

data Agent = Agent
  { name :: Text
  , role :: Role
  , knowledge :: Knowledge
  , params :: Parameters
  } deriving stock (Eq, Ord, Show, Generic)

data Parameters = Parameters
  { seerCentreChance :: Double
  , robberActChance :: Double
  , troublemakerActChance :: Double
  } deriving stock (Eq, Ord, Show, Generic)

defaultParams :: Parameters
defaultParams = Parameters
  { seerCentreChance = 0.2
  , robberActChance = 1
  , troublemakerActChance = 1
  }

data Game = Game
  { agents :: M.Map Text Agent
  , centre :: (Role, Role, Role)
  , time :: Int
  } deriving stock (Eq, Ord, Generic)

instance Display Game where
  displayBuilder Game{..} = mconcat $ ["At time ", show time, " the centre cards are ", show centre, " and the players are: \n"] <> one (TB.fromText $
    T.intercalate ", " $ map (\a -> mconcat [name a, " (", show (role a), ")"]) (M.elems agents))

makeFieldLabelsNoPrefix ''Game
makeFieldLabelsNoPrefix ''Agent

makeGame :: [(Text, Role)] -> Game
makeGame setup =
  let ([(_, c1), (_, c2), (_, c3)], a) = splitAt 3 setup
  in
  Game
    { agents = M.fromList . fmapToFst name . map (uncurry makeAgent) $ a
    , centre = (c1, c2, c3)
    , time = 0
    }

threeSetup :: [Role]
threeSetup = [Werewolf, Werewolf, Seer, Robber, Troublemaker, Villager]

fourSetup :: [Role]
fourSetup = Villager : threeSetup

fiveSetup :: [Role]
fiveSetup = Villager : fourSetup

makeAgent :: Text -> Role -> Agent
makeAgent n r = Agent { name = n, role = r, knowledge = one (0, AgentIsRole n r), params = defaultParams }

doNight :: [Role] -> IO ()
doNight r = void $ runEff $ do
  r' <- liftIO $ shuffleM r
  n <- liftIO $ shuffleM names
  let g = makeGame (zip (take (length r') n) r')
  execStateLocal g $ do

    #time .= 0
    werewolvesAwaken
    seerAwakens
    robberAwakens
    troublemakerAwakens
    showGame

showGame :: State Game :> es => IOE :> es => Eff es ()
showGame = do
  g' <- get
  log (display g')

log :: IOE :> es => Text -> Eff es ()
log = putStrLn . toString

advanceTime :: State Game :> es => Eff es ()
advanceTime = #time %= (+1)

seerAwakens :: IOE :> es => State Game :> es => Eff es ()
seerAwakens = do
  w <- getByUniqueRole Seer
  whenJust w $ \w' -> do
    log $ "The seer " <> name w' <> " has awoken"
    r <- decideOn (seerCentreChance . params $ w')
    if r then do
      (i, c) <- randomCentreCard
      (i2, c2) <- whileM ((== i) . fst) randomCentreCard
      learn w' (CentreCardIsRole i c)
      learn w' (CentreCardIsRole i2 c2)
    else do
      (n, r') <- randomPlayerRole w'
      learn w' (AgentIsRole n r')


whileM :: Monad m => (a -> Bool) -> m a -> m a
whileM pr f = do
  a <- f
  if pr a then whileM pr f else return a

decideOn :: IOE :> es => Double -> Eff es Bool
decideOn d = do
  i <- liftIO $ getRandomR (0.0, 1.0)
  return $ i < d

robberAwakens :: IOE :> es => State Game :> es => Eff es ()
robberAwakens = do
  w <- getByUniqueRole Robber
  whenJust w $ \w' -> do
    log $ "The robber " <> name w' <> " has awoken"
    r <- decideOn (robberActChance . params $ w')
    when r $ do
      -- pick a random player
      (n, r') <- randomPlayerRole w'
      -- now they have the robber's card, and the robber has their card
      let (myN, myR) = (name w', role w')
      t <- use #time
      learnAtTime (t-1) w' (AgentIsRole n r')
      learn w' (AgentIsRole n myR)
      learn w' (AgentIsRole myN r')
      swapRoles myN n

troublemakerAwakens :: IOE :> es => State Game :> es => Eff es ()
troublemakerAwakens = do
  w <- getByUniqueRole Troublemaker
  whenJust w $ \w' -> do
    log $ "The troublemaker " <> name w' <> " has awoken"
    r <- decideOn (troublemakerActChance . params $ w')
    when r $ do
      -- pick two random players
      (n, r') <- randomPlayerRole w'
      (n2, r2) <- whileM ((== n) . fst) (randomPlayerRole w')
      -- no learnings
      swapRoles n n2
      log $ "The troublemaker has swapped " <> n <> " (" <> show r' <> ") and " <> n2 <> " (" <> show r2 <> ")"

getAgent :: State Game :> es => Text -> Eff es Agent
getAgent n = do
  s <- get
  let r1 = s ^? #agents % at n % _Just
  return (fromMaybe (error $ "could not find agent " <> n) r1)

modifyAgent :: State Game :> es => (Agent -> Agent) -> Text -> Eff es ()
modifyAgent f n = #agents % at n % _Just %= f

swapRoles :: State Game :> es => Text -> Text -> Eff es ()
swapRoles n1 n2 = do
  r1 <- getAgent n1
  r2 <- getAgent n2
  modifyAgent (#role .~ role r2) n1
  modifyAgent (#role .~ role r1) n2

werewolvesAwaken :: IOE :> es => State Game :> es => Eff es ()
werewolvesAwaken = do
  w <- getByRole Werewolf
  case w of
    [] -> log "No werewolves"
    [x] -> do
      log ("One werewolf " <> name x)
      lookAtCentre x
    [x, y] -> learn x (AgentIsRole (name y) Werewolf) >> learn y (AgentIsRole (name x) Werewolf)
    _ -> error "too many werewolves"
  advanceTime

lookAtCentre :: IOE :> es => State Game :> es => Agent -> Eff es ()
lookAtCentre a1 = do
  (i, c) <- randomCentreCard
  learn a1 (CentreCardIsRole i c)

randomCentreCard :: IOE :> es => State Game :> es => Eff es (Int, Role)
randomCentreCard = do
  i <- liftIO $ getRandomR (1, 3)
  c <- use $ #centre % centreL i
  return (i, c)

randomPlayerRole :: IOE :> es => State Game :> es => Agent -> Eff es (Text, Role)
randomPlayerRole a = do
  as <- M.elems <$> use #agents
  a' <- fromMaybe (error "impossible") . viaNonEmpty head <$> liftIO (shuffleM (delete a as))
  return (name a', role a')

getByRole :: State Game :> es => Role -> Eff es [Agent]
getByRole r = M.elems . M.filter ((== r) . role) <$> use #agents

getByUniqueRole :: State Game :> es => Role -> Eff es (Maybe Agent)
getByUniqueRole r = viaNonEmpty head . M.elems . M.filter ((== r) . role) <$> use #agents

learnAtTime  :: IOE :> es => State Game :> es => Int -> Agent -> Fact -> Eff es ()
learnAtTime t a1 r = do
  t' <- use #time
  let tdiff = t' - t
  log $ name a1 <> " has learned that " <> show r <> if tdiff > 0 then show tdiff <> " timesteps ago" else ""
  #agents % at (name a1) % _Just % #knowledge %= S.insert (t, r)

learn :: IOE :> es => State Game :> es => Agent -> Fact -> Eff es ()
learn a1 r = do
  t <- use #time
  learnAtTime t a1 r

