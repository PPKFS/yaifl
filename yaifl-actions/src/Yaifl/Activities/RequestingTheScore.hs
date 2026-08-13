module Yaifl.Activities.RequestingTheScore
  ( requestingTheScoreImpl
  , WithRequestingTheScore

  ) where

import Yaifl.Prelude
import Yaifl.Activity
import Yaifl.Rulebook
import Yaifl.Text.Say
import Yaifl.Metadata
import Yaifl.Effects.RuleEffects

type WithRequestingTheScore wm =
  ( WithActivity "requestingTheScore" wm () () ()
  )

type RequestingTheScoreRule wm = ActivityRule wm () () ()

requestingTheScoreImpl :: Activity wm () () ()
requestingTheScoreImpl = makeActivity "requesting the score" [makeRule "requesting the score" [] (const standardAnnounceTheScore) ]

standardAnnounceTheScore :: RuleEffects wm es => Eff es (Maybe ())
standardAnnounceTheScore = do
  hasEnded <- gameHasEnded
  score <- getScore
  turnCount <- getTurnCount
  maxScore <- fromMaybe 0 <$> getMaxScore
  hasMaxScore <- isJust <$> getMaxScore
  let multiTurns = turnCount > 0
  if isNothing score then [saying|There #{are} no score in this story. #{linebreak}|]
  else [saying|{?if hasEnded}In that game you scored{?else}You have so far scored{?end if} {score} {?if hasMaxScore}out of a possible {maxScore}{?end if}, in {turnCount} turn{?if multiTurns}s{?end if}. #{linebreak}|]
  rulePass