{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Lamp.Actions.Looking
  ( lookingAction
  ) where

import Yaifl.Core.Actions.Action
import Yaifl.Core.Actions.Activity
import Yaifl.Core.Entity
import Yaifl.Core.Logger
import Yaifl.Core.Metadata
import Yaifl.Core.Object
import Yaifl.Core.Objects.Query
import Yaifl.Core.Objects.ThingData
import Yaifl.Core.Rulebooks.Args
import Yaifl.Core.Rulebooks.Rule
import Yaifl.Core.Rulebooks.Rulebook
import Yaifl.Core.Say
import Yaifl.Lamp.Activities.PrintingNameOfSomething (printName, capitalThe, printNameEx)
import Yaifl.Lamp.Visibility
import qualified Data.Text as T
import qualified Prettyprinter.Render.Terminal as PPTTY
import Solitude
import Effectful
import Effectful.Optics
import Yaifl.Core.AdaptiveText.Eval

lookingAction ::
  HasLookingProperties wm
  => Action wm
lookingAction = Action
  "looking"
  ["look", "looking"]
  [] --todo: add "at => examine"
  (ParseArguments lookingActionSet)
  (makeActionRulebook "before looking rulebook" [])
  (makeActionRulebook "check looking rulebook" [])
  carryOutLookingRules
  (makeActionRulebook "report looking rulebook" [])

-- if we have no source, then we have no idea where we are looking 'from'; return nothing
-- lightLevels (recalc light) is how many levels we can actually see because of light
-- vl is how many levels we could see in perfect light.
-- so if there's no light at all, then we take none of the levels - even if we could potentially see
-- 100 up.
lookingActionSet ::
  HasLookingProperties wm
  => NoMissingObjects wm es
  => UnverifiedArgs wm
  -> Eff es (ArgumentParseResult (LookingActionVariables wm))
lookingActionSet (UnverifiedArgs Args{..}) = withoutMissingObjects (do
  -- loc may be a thing (a container) or a room (the more likely case)
  loc <- getObject (_argsSource ^. objData % thingContainedBy)
  vl <- getVisibilityLevels loc
  lightLevels <- recalculateLightOfParent _argsSource
  return $ Right $ LookingActionVariables loc lightLevels (take lightLevels vl) "looking")
    (handleMissingObject "Failed to set the variables for looking" $ Left "Failed to set the variables for looking")

carryOutLookingRules :: ActionRulebook wm (LookingActionVariables wm)
carryOutLookingRules = makeActionRulebook "Carry Out Looking" [
  makeRule "room description heading rule"
    (\rb -> do
      setStyle (Just PPTTY.bold)
      let (LookingActionVariables loc cnt lvls _) = _argsVariables rb
          visCeil = viaNonEmpty last lvls
      debug "todo: fix"--show $ _argsVariables rb
      debug [int|t|"Printing room description heading with visibility ceiling TODO and visibility count #{cnt}|]
      if
        | cnt == 0 -> do
          doActivity printingNameOfADarkRoom ()
          pass --no light, print darkness
        | (getID <$> visCeil) == Just (getID loc) ->
          traverse_ printName visCeil --if the ceiling is the location, then print [the location]
        | True ->
          traverse_ (printNameEx capitalThe) visCeil --otherwise print [The visibility ceiling]
      mapM_ foreachVisibilityHolder (drop 1 lvls)
      sayLn "\n"
      setStyle Nothing
      --TODO: "run paragraph on with special look spacing"?
      return Nothing),
  makeRule "room description body rule"
    (\rb -> do
      let (LookingActionVariables loc cnt lvls ac) = _argsVariables rb
          visCeil = viaNonEmpty last lvls
      roomDesc <- use roomDescriptions
      dw <- use darknessWitnessed
      let abbrev = roomDesc == AbbreviatedRoomDescriptions
          someAbbrev = roomDesc == SometimesAbbreviatedRoomDescriptions
      if
        | cnt == 0 ->
          unless (abbrev || (someAbbrev && dw)) $ do
            doActivity printingDescriptionOfADarkRoom ()
            pass
        | (getID <$> visCeil) == Just (getID loc) ->
          unless (abbrev || (someAbbrev && ac /= "looking")) $ do
            desc <- evalDescription loc
            when (desc /= T.empty)
              (sayLn desc)
        | otherwise -> pass
      return Nothing),
  makeRule "room description paragraphs about objects rule"
    (\rb -> do
      let (LookingActionVariables _ _ lvls _) = _argsVariables rb

      mapM_ (\o -> doActivity describingLocale (LocaleVariables emptyStore o 0)) lvls
      return Nothing)
  ]