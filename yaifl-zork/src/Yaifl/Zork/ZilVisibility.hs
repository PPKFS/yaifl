{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
module Yaifl.Zork.ZilVisibility where

import Yaifl.Prelude

import Yaifl.Actions.Args
import Yaifl.Actions.Imports
import Yaifl.Effects.ObjectQuery
import Yaifl.Effects.RuleEffects
import Yaifl.Object.Query
import Yaifl.Object.Kind
import Yaifl.ObjectLike
import Yaifl.Thing.Kind
import Yaifl.Zork.Scoring (ZilVisibility(..))
import Yaifl.Zork.Specifics (ZorkWorldModel)

isZilVisible ::
  WithoutMissingObjects ZorkWorldModel es
  => ThingLike ZorkWorldModel o
  => o
  -> Eff es Bool
isZilVisible thing = do
  t <- getThing thing
  let visibility = t ^. #objectData % #thingData % #zilVisibility
  pure (visibility == ZilVisible)

setZilVisibility ::
  WithoutMissingObjects ZorkWorldModel es
  => ThingLike ZorkWorldModel o
  => ZilVisibility
  -> o
  -> Eff es ()
setZilVisibility visibility obj =
  modifyThing obj (#objectData % #thingData % #zilVisibility .~ visibility)

makeZilVisible :: WithoutMissingObjects ZorkWorldModel es
  => ThingLike ZorkWorldModel o => o -> Eff es ()
makeZilVisible = setZilVisibility ZilVisible

makeZilInvisible :: WithoutMissingObjects ZorkWorldModel es
  => ThingLike ZorkWorldModel o => o -> Eff es ()
makeZilInvisible = setZilVisibility ZilInvisible

checkZilVisibilityGlobal :: ArgsMightHaveMainObject v (Thing ZorkWorldModel) => RuleEffects ZorkWorldModel es => Args ZorkWorldModel v -> Eff es (Maybe Bool)
checkZilVisibilityGlobal args = do
  case preview (#variables % argsMainObjectMaybe @_ @(Thing ZorkWorldModel)) args of
    Just thing -> do
      let visibility = thing ^. #objectData % #thingData % #zilVisibility
      if visibility == ZilInvisible
        then do
          [saying|You can't see any such thing.|]
          pure (Just False)
        else rulePass
    Nothing -> rulePass

checkZilVisibilitySecondNoun :: ArgsMightHaveSecondObject v (Thing ZorkWorldModel) => RuleEffects ZorkWorldModel es => Args ZorkWorldModel v -> Eff es (Maybe Bool)
checkZilVisibilitySecondNoun args = do
  case preview (#variables % argsSecondObjectMaybe @_ @(Thing ZorkWorldModel)) args of
    Just thing -> do
      let visibility = thing ^. #objectData % #thingData % #zilVisibility
      if visibility == ZilInvisible
        then do
          [saying|You can't see any such thing.|]
          pure (Just False)
        else rulePass
    Nothing -> rulePass
