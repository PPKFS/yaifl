{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Lamp.Actions.Looking
  ( lookingAction
  , roomDescriptionHeadingAImpl
  , roomDescriptionHeadingBImpl
  , roomDescriptionHeadingCImpl
  ) where

import Solitude

import Breadcrumbs ( addTag )
import Data.Text.Display ( display )
import Effectful.Optics ( use )
import Yaifl.Core.Actions.Action
import Yaifl.Core.Actions.Activity
import Yaifl.Core.AdaptiveNarrative
import Yaifl.Core.Entity ( emptyStore, HasID(..) )
import Yaifl.Core.Metadata
import Yaifl.Core.Object ( Object(..), AnyObject )
import Yaifl.Core.Objects.Query
import Yaifl.Core.Objects.ThingData ( ThingData(..) )
import Yaifl.Core.Print ( Print, setStyle, printLn )
import Yaifl.Core.Rules.Args
import Yaifl.Core.Rules.Rule
import Yaifl.Core.Rules.Rulebook
import Yaifl.Lamp.Activities.DescribingLocale ( WithDescribingLocale )
import Yaifl.Core.SayQQ
import Yaifl.Lamp.Properties.Animal
import Yaifl.Lamp.Properties.Supporter ( isSupporter )
import Yaifl.Core.Responses
import Yaifl.Lamp.Say
import Yaifl.Lamp.Visibility
import qualified Data.Text as T
import qualified Prettyprinter.Render.Terminal as PPTTY
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Lamp.Locale

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
  return $ Right $ LookingActionVariables loc (take lightLevels vl) "looking")
    (handleMissingObject "Failed to set the variables for looking" $ Left "Failed to set the variables for looking")

carryOutLookingRules ::
  WithPrintingNameOfADarkRoom wm
  => WithDescribingLocale wm
  => WithResponse wm "roomDescriptionHeadingA" ()
  => WithResponse wm "roomDescriptionHeadingB" (AnyObject wm)
  => WithResponse wm "roomDescriptionHeadingC" (AnyObject wm)
  => WithResponse wm "roomDescriptionBodyA" ()
  => WithPrintingDescriptionOfADarkRoom wm
  => ActionRulebook wm (LookingActionVariables wm)
carryOutLookingRules = makeActionRulebook "carry out looking" [
  makeRule "room description heading rule"
    (\rb -> do
      setStyle (Just PPTTY.bold)
      let (LookingActionVariables loc lvls _) = variables rb
          mbVisCeil = viaNonEmpty last lvls
      whenJust mbVisCeil $ addTag "visibility ceiling" . display
      case mbVisCeil of
        -- vis count is 0
        Nothing -> do
          beginActivity #printingNameOfADarkRoom ()
          whenHandling' #printingNameOfADarkRoom $ do
            -- "Darkness"
            sayResponse #roomDescriptionHeadingA ()
          endActivity #printingNameOfADarkRoom
        Just visCeil ->
          if getID visCeil == getID loc
          then do
            addTag @Text "Ceiling is the location" ""
            [saying|{visCeil}|]
          else do
            addTag @Text "Ceiling is not the location" ""
            [saying|{The visCeil}|]
      mapM_ foreachVisibilityHolder (drop 1 lvls)
      printLn "\n"
      setStyle Nothing
      --TODO: "run paragraph on with special look spacing"?
      return Nothing),

  makeRule "room description body rule"
    (\rb -> do
      let (LookingActionVariables loc lvls ac) = variables rb
          mbVisCeil = viaNonEmpty last lvls
      roomDesc <- use @Metadata #roomDescriptions
      dw <- use @Metadata #darknessWitnessed
      addTag "darkness witnessed" dw
      addTag "room descriptions" roomDesc
      let abbrev = roomDesc == AbbreviatedRoomDescriptions
          someAbbrev = roomDesc == SometimesAbbreviatedRoomDescriptions
      case mbVisCeil of
        -- vis count is 0
        Nothing ->
          unless (abbrev || (someAbbrev && dw)) $ do
            beginActivity #printingDescriptionOfADarkRoom ()
            whenHandling' #printingDescriptionOfADarkRoom $ do
              regarding Nothing
              [saying|#{It} #{are} pitch dark, and #{we} #{can't see} a thing.|]
              sayResponse #roomDescriptionBodyA ()
            endActivity #printingNameOfADarkRoom
        Just visCeil ->
          if getID visCeil == getID loc
          then
            unless (abbrev || (someAbbrev && ac /= "looking")) $ do
              desc <- sayText $ description loc
              when (display desc /= T.empty)
                (printLn $ display desc)
          else
            pass
      return Nothing),

  makeRule "room description paragraphs about objects rule"
    (\rb -> do
      let (LookingActionVariables _ lvls _) = variables rb
      mapM_ (\o -> doActivity #describingLocale (LocaleVariables emptyStore o 0)) lvls
      return Nothing)
  ]

foreachVisibilityHolder ::
  NoMissingObjects wm es
  => ActionHandler wm :> es
  => ObjectTraverse wm :> es
  => Print :> es
  => State (ActivityCollector wm) :> es
  => State (AdaptiveNarrative wm) :> es
  => State (ResponseCollector wm) :> es
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

roomDescriptionHeadingAImpl :: Response wm ()
roomDescriptionHeadingAImpl = Response $ const [sayingTell|Darkness|]

roomDescriptionHeadingBImpl ::
  WithPrintingNameOfSomething wm
  => Response wm (AnyObject wm)
roomDescriptionHeadingBImpl = Response $ \intermediateLevel -> [sayingTell|(on {the intermediateLevel})|]

roomDescriptionHeadingCImpl ::
  WithPrintingNameOfSomething wm
  => Response wm (AnyObject wm)
roomDescriptionHeadingCImpl = Response $ \intermediateLevel -> [sayingTell|(in {the intermediateLevel})|]