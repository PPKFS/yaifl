{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Actions.Looking
  ( lookingAction
  , RoomDescriptionResponses(..)
  , roomDescriptionResponsesImpl
  ) where

import Solitude

import Breadcrumbs ( addTag )
import Data.Text.Display ( display )
import Effectful.Optics ( use )

import Yaifl.Actions.Action
import Yaifl.Actions.Looking.Locale
import Yaifl.Actions.Looking.Visibility
import Yaifl.Activities.Activity
import Yaifl.Activities.PrintingTheLocaleDescription ( WithPrintingTheLocaleDescription )
import Yaifl.Metadata
import Yaifl.Model.Entity ( emptyStore, HasID(..) )
import Yaifl.Model.Object( Object(..), AnyObject, Thing, objectEquals )
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Objects.Query
import Yaifl.Model.Objects.RoomData ( IsVisited(..) )
import Yaifl.Model.Objects.ThingData ( ThingData(..) )
import Yaifl.Model.Properties.Animal
import Yaifl.Model.Properties.Supporter ( isSupporter )
import Yaifl.Rules.Args
import Yaifl.Rules.Rule
import Yaifl.Rules.RuleEffects
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.Print ( Print, setStyle, printLn )
import Yaifl.Text.Responses
import Yaifl.Text.Say
import Yaifl.Text.SayQQ
import qualified Prettyprinter.Render.Terminal as PPTTY

-- STATUS: all done, except report other people looking
-- some of the locale printing needs more work
roomDescriptionResponsesImpl :: WithPrintingNameOfSomething wm => RoomDescriptionResponses wm
roomDescriptionResponsesImpl = RDR
  { roomDescriptionHeadingA = Response $ const [sayingTell|Darkness|]
  , roomDescriptionHeadingB = Response $ \intermediateLevel -> [sayingTell|(on {the intermediateLevel})|]
  , roomDescriptionHeadingC = Response $ \intermediateLevel -> [sayingTell|(in {the intermediateLevel})|]
  , roomDescriptionBodyA = Response $ const [sayingTell|#{It} #{are} pitch dark, and #{we} #{can't see} a thing.|]
  }

lookingAction ::
  HasLookingProperties wm
  => Action wm ('Optionally 'TakesConstantParameter) (LookingActionVariables wm)
lookingAction = Action
  "looking"
  ["look", "looking"]
  [] --todo: add "at => examine"
  -- if we have no source, then we have no idea where we are looking 'from'; return nothing
-- lightLevels (recalc light) is how many levels we can actually see because of light
-- vl is how many levels we could see in perfect light.
-- so if there's no light at all, then we take none of the levels - even if we could potentially see
-- 100 up.
  (ParseArguments $ \(UnverifiedArgs Args{..}) -> do
    -- loc may be a thing (a container) or a room (the more likely case)
    loc <- getObject (source ^. #objectData % #containedBy)
    vl <- getVisibilityLevels loc
    lightLevels <- recalculateLightOfParent source
    acName <- case fst variables of
      Nothing -> pure "looking"
      Just acName -> pure acName
    return $ Right $ LookingActionVariables loc (take lightLevels vl) acName)
  (makeActionRulebook "before looking rulebook" [])
  (makeActionRulebook "check looking rulebook" [])
  carryOutLookingRules
  (makeActionRulebook "report looking rulebook" [])


carryOutLookingRules ::
  WithPrintingNameOfADarkRoom wm
  => WithPrintingTheLocaleDescription wm
  => WithResponseSet wm "roomDescriptions" (RoomDescriptionResponses wm)
  => WithPrintingDescriptionOfADarkRoom wm
  => ActionRulebook wm (LookingActionVariables wm)
carryOutLookingRules = makeActionRulebook "carry out looking" [
  makeRule "room description heading rule" forPlayer'
    (\a@Args{variables=(LookingActionVariables _ lvls _)} -> do
      -- say bold type;
      setStyle (Just PPTTY.bold)
      let mbVisCeil = viaNonEmpty last lvls
      whenJust mbVisCeil $ addTag "visibility ceiling" . display
      loc <- getActorLocation a
      case mbVisCeil of
        -- if the visibility level count is 0:
        Nothing -> do
          --begin the printing the name of a dark room activity;      
          beginActivity #printingNameOfADarkRoom ()
          -- if handling the printing the name of a dark room activity:
          whenHandling' #printingNameOfADarkRoom $ do
            -- say "Darkness" (A);
            sayResponse (#roomDescriptions % #roomDescriptionHeadingA) ()
          -- end the printing the name of a dark room activity;
          void $ endActivity #printingNameOfADarkRoom
        Just visCeil ->
          -- otherwise if the visibility ceiling is the location:
          if getID visCeil == getID loc
          then do
            addTag @Text "Ceiling is the location" ""
            -- say "[visibility ceiling]";
            [saying|{visCeil}|]
          -- otherwise:
          else do
            addTag @Text "Ceiling is not the location" ""
            --  say "[The visibility ceiling]";
            [saying|{The visCeil}|]
      let
      -- repeat with intermediate level count running from 2 to the visibility level count:
      mapM_ foreachVisibilityHolder (drop 1 lvls)
      -- say line break;
      printLn "\n"
      setStyle Nothing
      --TODO: "run paragraph on with special look spacing"?
      return Nothing),

  makeRule "room description body rule" forPlayer'
    (\a@Args{variables=(LookingActionVariables _ lvls ac)} -> do
      let mbVisCeil = viaNonEmpty last lvls
      roomDesc <- use @Metadata #roomDescriptions
      dw <- use @Metadata #darknessWitnessed
      addTag "darkness witnessed" dw
      addTag "room descriptions" roomDesc
      let abbrev = roomDesc == AbbreviatedRoomDescriptions
          someAbbrev = roomDesc == SometimesAbbreviatedRoomDescriptions
      loc <- getActorLocation a
      case mbVisCeil of
        -- if the visibility level count is 0:
        Nothing ->
          -- if set to abbreviated room descriptions, continue the action;
          -- if set to sometimes abbreviated room descriptions and abbreviated
          -- form allowed is true and darkness witnessed is true, continue the action;
          unless (abbrev || (someAbbrev && dw)) $ do
            -- begin the printing the description of a dark room activity;
            beginActivity #printingDescriptionOfADarkRoom ()
            -- if handling the printing the description of a dark room activity:
            whenHandling' #printingDescriptionOfADarkRoom $ do
              -- now the prior named object is nothing;
              regarding Nothing
              -- say "[It] [are] pitch dark, and [we] [can't see] a thing." (A);
              sayResponse (#roomDescriptions % #roomDescriptionBodyA) ()
            -- end the printing the description of a dark room activity;
            void $ endActivity #printingDescriptionOfADarkRoom
        Just visCeil ->
          -- otherwise if the visibility ceiling is the location:
          when (visCeil `objectEquals` loc) $ do
            -- if set to abbreviated room descriptions, continue the action;
            -- if set to sometimes abbreviated room descriptions and abbreviated form allowed
            -- is true and the location is visited, continue the action;
            unless (abbrev || (someAbbrev && ac /= "looking" && loc ^. #objectData % #isVisited == Visited)) $
              -- print the location's description;
              sayLn $ loc ^. #description
      return Nothing),
  -- because I've ignored all the junk about marked for listing or w/e, and we can do nice clean loops
  -- 19 lines down to 2. lol.
  -- it is a very long-winded way to iterate through the visibility levels and describe the locale at each level.
  -- the actual meat is
  -- describe locale for the intermediate position;
  makeRule "room description paragraphs about objects rule" forPlayer'
    (\rb -> mapM_ (\o -> doActivity #printingTheLocaleDescription (LocaleVariables emptyStore o 0)) (visibilityLevels . variables $ rb) >>
      return Nothing),

  makeRule "check new arrival rule" forPlayer'
    (\a@Args{variables=(LookingActionVariables _ lvls _)} -> do
      let mbVisCeil = viaNonEmpty last lvls
      case mbVisCeil of
        -- if in darkness:
        Nothing ->
          -- now the darkness witnessed is true;
          modify @Metadata (#darknessWitnessed .~ True) >> rulePass
        Just _ -> do
          -- if the location is a room, now the location is visited;
          -- except...a location is always a room.
          loc <- getActorLocation a
          modifyRoom loc (#objectData % #isVisited .~ Visited)  >> rulePass
      )
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
  => WithResponseSet wm "roomDescriptions" (RoomDescriptionResponses wm)
  => AnyObject wm
  -> Eff es ()
foreachVisibilityHolder e = do
  -- let intermediate level be the visibility-holder of the actor;   
  -- repeat with intermediate level count running from 2 to the visibility level count:
  ifM (isSupporter e  ||^ isAnimal e)
    -- say " (on [the intermediate level])" (B);
    (sayResponse (#roomDescriptions % #roomDescriptionHeadingB) e)
    -- say " (in [the intermediate level])" (C);    
    (sayResponse (#roomDescriptions % #roomDescriptionHeadingC) e)

otherPeopleLookingAImpl ::
  WithPrintingNameOfSomething wm
  => Response wm (Thing wm)
otherPeopleLookingAImpl = Response $ \actor -> [saying|{The actor} #{look} around.|]