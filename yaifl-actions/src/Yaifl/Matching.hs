module Yaifl.Matching
  ( getMatchingThing
  , getMatching

  ) where
import Yaifl.Prelude
import Yaifl.Effects.RuleEffects
import Yaifl.Actions.Args
import Yaifl.Thing.Kind
import Yaifl.Actions.GoesWith
import Yaifl.Object.Query

getMatchingThing ::
  RuleEffects wm es
  => Text
  -> UnverifiedArgs wm params
  -> Eff es (Maybe (Thing wm))
getMatchingThing matchElement args = do
  e <- getMatching matchElement args
  case e of
    Just (ObjectParameter o) -> getThingMaybe o
    Just (ThingParameter t) -> return (Just t)
    _ -> return Nothing

getMatching :: Text -> UnverifiedArgs wm params -> Eff es (Maybe (ActionParameter wm))
getMatching matchElement (UnverifiedArgs args) = do
  let mbMatch = args ^? #variables % _2 % to (find ((== matchElement) . fst) ) % _Just % _2
  return mbMatch
