{-# LANGUAGE RecordWildCards #-}

module Yaifl.Game.Actions.Looking
  ( lookingAction
  , LookingResponses(..)
  , roomDescriptionResponsesImpl
  ) where

import Yaifl.Prelude hiding ( Reader )

import Breadcrumbs ( addTag, addAnnotation )

import Effectful.Reader.Static
import Yaifl.Model.Action
import Yaifl.Game.Actions.Looking.Locale
import Yaifl.Game.Actions.Looking.Visibility
import Yaifl.Model.Activity
import Yaifl.Model.Metadata
import Yaifl.Model.Kinds.Object( Object(..), objectEquals )
import Yaifl.Model.Effects
import Yaifl.Model.Entity ( HasID(..) )
import Yaifl.Model.Query
import Yaifl.Model.Kinds.Room ( IsVisited(..) )
import Yaifl.Model.Store
import Yaifl.Model.Kinds.Animal
import Yaifl.Model.Kinds.Supporter ( isSupporter )
import Yaifl.Model.Actions.Args
import Yaifl.Model.Rules.Rulebook
import Yaifl.Model.Rules.RuleEffects
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.Print ( Print, setStyle, runOnLookingParagraph )
import Yaifl.Text.Responses
import Yaifl.Text.Say
import qualified Prettyprinter.Render.Terminal as PPTTY
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Kinds.Thing
import Yaifl.Model.Input

data LookingResponses wm =
  RoomDescriptionHeadingA
  | RoomDescriptionHeadingB (AnyObject wm)
  | RoomDescriptionHeadingC (AnyObject wm)
  | RoomDescriptionBodyA
  deriving stock (Generic)

-- STATUS: all done, except report other people looking
roomDescriptionResponsesImpl :: WithPrintingNameOfSomething wm => LookingResponses wm -> Response wm (Args wm (LookingActionVariables wm))
roomDescriptionResponsesImpl = \case
  RoomDescriptionHeadingA -> Response $ const [sayingTell|Darkness|]
  RoomDescriptionHeadingB intermediateLevel -> Response $ \_ -> [sayingTell| (on {the intermediateLevel})|]
  RoomDescriptionHeadingC intermediateLevel -> Response $ \_ -> [sayingTell| (in {the intermediateLevel})|]
  RoomDescriptionBodyA -> Response $ const [sayingTell|#{It} #{are} pitch dark, and #{we} #{can't see} a thing.|]

type LookingAction wm = Action wm (LookingResponses wm) ('Optionally 'TakesConstantParameter) (LookingActionVariables wm)
type LookingRule wm = ActionRule wm (LookingAction wm) (LookingActionVariables wm)

lookingAction ::
  HasLookingProperties wm
  => LookingAction wm
lookingAction = (makeAction "looking")
  { understandAs = ["look", "looking"]
  , matches = [] --todo: add "at => examine"
  , responses = roomDescriptionResponsesImpl
  , parseArguments = ParseArguments $ \(UnverifiedArgs Args{..}) -> do
    -- if we have no source, then we have no idea where we are looking 'from'; return nothing
    -- lightLevels (recalc light) is how many levels we can actually see because of light
    -- vl is how many levels we could see in perfect light.
    -- so if there's no light at all, then we take none of the levels - even if we could potentially see
    -- 100 up.
      -- loc may be a thing (a container) or a room (the more likely case)
      loc <- getObject (source ^. #objectData % #containedBy)
      addAnnotation $ "actor is located at " <> display loc
      vl <- getVisibilityLevels loc
      lightLevels <- recalculateLightOfParent source
      acName <- case fst variables of
        Nothing -> pure "looking"
        Just acName -> pure acName
      return $ SuccessfulParse $ LookingActionVariables loc (take lightLevels vl) acName
  , carryOutRules = makeActionRulebook "carry out looking"
        [ roomDescriptionHeading
        , roomDescriptionBody
        , aboutObjects
        , checkNewArrival
        ]
  }

roomDescriptionHeading ::
  HasLookingProperties wm
  => LookingRule wm
roomDescriptionHeading = makeRule "room description heading rule" forPlayer'
    (\a@Args{variables=(LookingActionVariables _ lvls _)} -> do
      -- say bold type;
      setStyle (Just PPTTY.bold)
      addAnnotation $ "levels " <> mconcat (map display lvls)
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
            sayResponse RoomDescriptionHeadingA a
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
      -- repeat with intermediate level count running from 2 to the visibility level count:
      mapM_ (foreachVisibilityHolder a) (fromMaybe [] $ viaNonEmpty init lvls)
      -- say line break;
      [saying|#{linebreak}|]
      runOnLookingParagraph
      setStyle Nothing
      return Nothing)

foreachVisibilityHolder ::
  NoMissingObjects wm es
  => ActionHandler wm :> es
  => ObjectTraverse wm :> es
  => Print :> es
  => Input :> es
  => Reader (LookingAction wm) :> es
  => State (ActivityCollector wm) :> es
  => State (AdaptiveNarrative wm) :> es
  => State (ResponseCollector wm) :> es
  => WithPrintingNameOfSomething wm
  => Args wm (LookingActionVariables wm)
  -> AnyObject wm
  -> Eff es ()
foreachVisibilityHolder a e = do
  -- let intermediate level be the visibility-holder of the actor;   
  -- repeat with intermediate level count running from 2 to the visibility level count:
  ifM (isSupporter e  ||^ isAnimal e)
    -- say " (on [the intermediate level])" (B);
    (sayResponse (RoomDescriptionHeadingB e) a)
    -- say " (in [the intermediate level])" (C);    
    (sayResponse (RoomDescriptionHeadingC e) a)

roomDescriptionBody ::
  HasLookingProperties wm
  => LookingRule wm
roomDescriptionBody = makeRule "room description body rule" forPlayer'
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
          unless (abbrev || someAbbrev && dw) $ do
            -- begin the printing the description of a dark room activity;
            beginActivity #printingDescriptionOfADarkRoom ()
            -- if handling the printing the description of a dark room activity:
            whenHandling' #printingDescriptionOfADarkRoom $ do
              -- now the prior named object is nothing;
              regardingNothing
              -- say "[It] [are] pitch dark, and [we] [can't see] a thing." (A);
              sayResponse RoomDescriptionBodyA a
            -- end the printing the description of a dark room activity;
            void $ endActivity #printingDescriptionOfADarkRoom
        Just visCeil ->
          -- otherwise if the visibility ceiling is the location:
          when (visCeil `objectEquals` loc) $ do
            -- if set to abbreviated room descriptions, continue the action;
            -- if set to sometimes abbreviated room descriptions and abbreviated form allowed
            -- is true and the location is visited, continue the action;
            unless (abbrev || someAbbrev && ac /= "looking" && loc ^. #objectData % #isVisited == Visited) $
              -- print the location's description;
              say $ loc ^. #description
      return Nothing)

  -- because I've ignored all the junk about marked for listing or w/e, and we can do nice clean loops
  -- 19 lines down to 2. lol.
  -- it is a very long-winded way to iterate through the visibility levels and describe the locale at each level.
  -- the actual meat is
  -- describe locale for the intermediate position;
aboutObjects ::
  HasLookingProperties wm
  => LookingRule wm
aboutObjects = makeRule "room description paragraphs about objects rule" forPlayer'
    (\rb -> mapM_ (\o -> doActivity #printingTheLocaleDescription (LocaleVariables emptyStore o 0)) (visibilityLevels . variables $ rb) >>
      return Nothing)

checkNewArrival ::
  HasLookingProperties wm
  => LookingRule wm
checkNewArrival = makeRule "check new arrival rule" forPlayer'
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

otherPeopleLookingAImpl ::
  WithPrintingNameOfSomething wm
  => Response wm (Thing wm)
otherPeopleLookingAImpl = Response $ \actor -> [saying|{The actor} #{look} around.|]