module Yaifl.Zork.Scoring where

import Yaifl.Prelude
import Yaifl.Actions.Imports
import Yaifl
import Yaifl.Effects.Print
import Yaifl.Create.Rule
import Yaifl.Activities.PrintingThePlayersObituary
import Yaifl.Activities.RequestingTheScore
import Yaifl.Effects.RuleEffects
import Yaifl.Zork.Metadata (ZorkData)
import Yaifl.Thing.Kind
import Yaifl.Object.Kind
import Yaifl.Object.Query (modifyThing)
import Yaifl.Effects.Interpreters

rankings :: [(Int, Text)]
rankings =
  [ (350, "Master Adventurer")
  , (330, "Wizard")
  , (300, "Master")
  , (200, "Adventurer")
  , (100, "Junior Adventurer")
  , (50, "Novice Adventurer")
  , (25, "Amateur Adventurer")
  , (0, "Beginner")
  ]

scoreToRank :: Int -> Text
scoreToRank score = maybe "Beginner" snd (find (\x -> score >= fst x) rankings)
scoreAndRankRule :: Rule' wm () r
scoreAndRankRule = makeRule' "score and rank rule" $ do
  score <- getScore
  turnCount <- getTurnCount
  let multipleTurns = turnCount > 1
      rank = scoreToRank score
  [saying|Your score is {score} (total of 350 points), in {turnCount} move{?if multipleTurns}s{?end if}.#{linebreak}This gives you the rank of {rank}.#{linebreak}|]
  rulePass

scoring ::
  WMThingData wm ~ ZorkThingData
  => WMValues wm ~ ZorkData
  => WithRequestingTheScore wm
  => WithPrintingThePlayersObituary wm
  => SayableValue (WMText wm) wm => WorldConstruction wm ()
scoring = do
  (#score :: Lens' (Metadata wm) Score) % #maxScore .= Just 350
  afterActivity' #printingThePlayersObituary [] "score and rank" $ do
    printLinebreak
    runRule scoreAndRankRule ()
    pass
  carryOutActivity #requestingTheScore [] "requesting the score" $ const $ do
    playerIsDead <- getValue #playerIsDead
    if playerIsDead
    then [saying|You're dead! How can you think of your score?|] >> return (Just ())
    else runRule scoreAndRankRule () >> return (Just ())

  after #taking [anObjectWithPoints] "first-take scoring rule" $ \Args{variables} -> do
    let thingScore = variables ^. #objectData % #thingData % #pointValue
    (#score :: Lens' (Metadata wm) Score) % #currentScore %= (+ coerce thingScore)
    modifyThing variables (#objectData % #thingData % #pointValue .~ PointValue 0)
    rulePass

anObjectWithPoints :: (WMThingData wm ~ ZorkThingData) => Precondition wm (Args wm (Thing wm))
anObjectWithPoints = Precondition
  { preconditionName = (pure "an object with a points value")
  , checkPrecondition = \args -> do
      let thingValue = (variables args) ^. #objectData % #thingData % #pointValue
      pure $ thingValue > 0
  }

data ZorkThingData = ZorkThingData
  { pointValue :: PointValue
  , treasureValue :: TreasureValue
  } deriving stock (Eq, Ord, Show, Generic)

instance Pointed ZorkThingData where
  identityElement = ZorkThingData defaultPointValue defaultTreasureValue
newtype TreasureValue = TreasureValue { unValue :: Int }
  deriving newtype (Eq, Ord, Num, Show)
  deriving stock (Generic)

defaultTreasureValue :: TreasureValue
defaultTreasureValue = TreasureValue 0

newtype PointValue = PointValue { unValue :: Int }
  deriving newtype (Eq, Ord, Num, Show)

defaultPointValue :: PointValue
defaultPointValue = PointValue 0

instance Pointed TreasureValue where
  identityElement = defaultTreasureValue

instance Pointed PointValue where
  identityElement = defaultPointValue

makeFieldLabelsNoPrefix ''ZorkThingData
makeFieldLabelsNoPrefix ''TreasureValue
makeFieldLabelsNoPrefix ''PointValue