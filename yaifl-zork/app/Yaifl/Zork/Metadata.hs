module Yaifl.Zork.Metadata where

import Yaifl.Prelude

data ZorkData = ZorkData
  { trollFlag :: Bool
  , magicFlag :: Bool
  , cyclopsFlag :: Bool
  , domeFlag :: Bool
  , lldFlag :: Bool
  , lowTideFlag :: Bool
  , rainbowFlag :: Bool
  , wonFlag :: Bool
  , grateRevealed :: Bool
  , gateFlag :: Bool
  , gatesOpenFlag :: Bool
  , luckyFlag :: Bool

  , playerDeaths :: Int
  , playerIsDead :: Bool
  , alwaysLitMode :: Bool

  , trophyCaseScore :: Int
  } deriving stock (Eq, Ord, Generic, Show)

makeFieldLabelsNoPrefix ''ZorkData

defaultZorkValues :: ZorkData
defaultZorkValues = ZorkData
  { trollFlag = False
  , magicFlag = False
  , cyclopsFlag = False
  , domeFlag = False
  , lldFlag = False
  , lowTideFlag = False
  , rainbowFlag = False
  , wonFlag = False
  , grateRevealed = False
  , gateFlag = False
  , gatesOpenFlag = False
  , luckyFlag = True

  , playerDeaths = 0
  , playerIsDead = False
  , alwaysLitMode = False

  , trophyCaseScore = 0
  }