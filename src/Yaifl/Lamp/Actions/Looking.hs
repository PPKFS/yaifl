{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Lamp.Actions.Looking
  ( lookingAction
  ) where

import Solitude
import Breadcrumbs ( addTag )

import qualified Data.Text as T
import qualified Prettyprinter.Render.Terminal as PPTTY
import Data.Text.Display ( display )

import Effectful.Optics ( use )

import Yaifl.Core.Actions.Action
import Yaifl.Core.Actions.Activity
import Yaifl.Core.Entity ( emptyStore, HasID(..) )
import Yaifl.Core.Metadata
import Yaifl.Core.Object ( Object(..), AnyObject )
import Yaifl.Core.Objects.Query
import Yaifl.Core.Objects.ThingData ( ThingData(..) )
import Yaifl.Core.Print ( Print, setStyle, printLn )
import Yaifl.Core.Rulebooks.Args
import Yaifl.Core.Rulebooks.Rule
import Yaifl.Core.Rulebooks.Rulebook
import Yaifl.Lamp.Activities.DescribingLocale ( WithDescribingLocale )
import Yaifl.Lamp.Properties.Animal
import Yaifl.Lamp.Properties.Supporter ( isSupporter )
import Yaifl.Lamp.Say
import Yaifl.Lamp.Visibility
import Yaifl.Core.AdaptiveNarrative

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
  loc <- getObject (source ^. #objectData % #containedBy)
  vl <- getVisibilityLevels loc
  lightLevels <- recalculateLightOfParent source
  return $ Right $ LookingActionVariables loc lightLevels (take lightLevels vl) "looking")
    (handleMissingObject "Failed to set the variables for looking" $ Left "Failed to set the variables for looking")

carryOutLookingRules ::
  WithPrintingNameOfADarkRoom wm
  => WithDescribingLocale wm
  => WithResponse wm "roomDescriptionHeadingA" ()
  => WithResponse wm "roomDescriptionHeadingB" (AnyObject wm)
  => WithResponse wm "roomDescriptionHeadingC" (AnyObject wm)
  => WithPrintingDescriptionOfADarkRoom wm
  => ActionRulebook wm (LookingActionVariables wm)
carryOutLookingRules = makeActionRulebook "carry out looking" [
  makeRule "room description heading rule"
    (\rb -> do
      setStyle (Just PPTTY.bold)
      let (LookingActionVariables loc cnt lvls _) = variables rb
          visCeil = viaNonEmpty last lvls
      whenJust visCeil $ addTag "visibility ceiling" . display
      if
        | cnt == 0 -> do
          beginActivity #printingNameOfADarkRoom ()
          whenHandling' #printingNameOfADarkRoom $ do
            -- "Darkness"
            sayResponse #roomDescriptionHeadingA ()
          endActivity #printingNameOfADarkRoom
          pass
        | (getID <$> visCeil) == Just (getID loc) -> do
          addTag @Text "Ceiling is the location" ""
          say visCeil
        | True -> do
          addTag @Text "Ceiling is not the location" ""
          say @Text "The " >> say visCeil
      mapM_ foreachVisibilityHolder (drop 1 lvls)
      printLn "\n"
      setStyle Nothing
      --TODO: "run paragraph on with special look spacing"?
      return Nothing),

  makeRule "room description body rule"
    (\rb -> do
      let (LookingActionVariables loc cnt lvls ac) = variables rb
          visCeil = viaNonEmpty last lvls
      roomDesc <- use @Metadata @RoomDescriptions #roomDescriptions
      dw <- use @Metadata @Bool #darknessWitnessed
      addTag "darkness witnessed" dw
      addTag "room descriptions" roomDesc
      let abbrev = roomDesc == AbbreviatedRoomDescriptions
          someAbbrev = roomDesc == SometimesAbbreviatedRoomDescriptions
      if
        | cnt == 0 ->
          unless (abbrev || (someAbbrev && dw)) $ do
            beginActivity #printingDescriptionOfADarkRoom ()
            -- HERE
            whenHandling' #printingDescriptionOfADarkRoom $ do
              say @Text "Darkness"
            endActivity #printingNameOfADarkRoom
        | (getID <$> visCeil) == Just (getID loc) ->
          unless (abbrev || (someAbbrev && ac /= "looking")) $ do
            desc <- sayText $ description loc
            when (display desc /= T.empty)
              (printLn $ display desc)
        | otherwise -> pass
      return Nothing),
  makeRule "room description paragraphs about objects rule"
    (\rb -> do
      let (LookingActionVariables _ _ lvls _) = variables rb
      mapM_ (\o -> doActivity #describingLocale (LocaleVariables emptyStore o 0)) lvls
      return Nothing)
  ]

foreachVisibilityHolder ::
  (NoMissingObjects wm es, ActionHandler wm :> es)
  => Print :> es
  => ObjectTraverse wm :> es
  => State (ActivityCollector wm) :> es
  => State (ResponseCollector wm) :> es
  => State (AdaptiveNarrative wm) :> es
  => WithPrintingNameOfSomething wm
  => WithResponse wm "roomDescriptionHeadingB" (AnyObject wm)
  => WithResponse wm "roomDescriptionHeadingC" (AnyObject wm)
  => AnyObject wm
  -> Eff es ()
foreachVisibilityHolder e = do
  ifM (isSupporter e  ||^ isAnimal e )
    -- say " (on [the intermediate level])" (B);
    (sayResponse #roomDescriptionHeadingB e)
    -- say " (in [the intermediate level])" (C);    
    (sayResponse #roomDescriptionHeadingC e)
  printName e
  say @Text ")"