{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module Yaifl.Actions.Entering where

import Yaifl.Actions.Imports
import Yaifl.Prelude
import Yaifl.Enclosing.Kind
import Yaifl.Move
import Yaifl.Container.Kind
import Yaifl.Object.Query
import Yaifl.AnyObject
import Yaifl.Metadata
import Effectful.Error.Static
import Yaifl.Activity
import Yaifl.Store
import Yaifl.Locale
import Yaifl.Activities.PrintingTheLocaleDescription (WithPrintingTheLocaleDescription)
import Yaifl.ObjectLike
import Yaifl.Thing.Kind
import Yaifl.Refreshable
import Yaifl.Object.Kind
import Yaifl.Enclosing.Query
import Yaifl.Effects.RuleEffects
import Yaifl.Door.Kind
import Yaifl.Property.Has
import Yaifl.MultiLocated.Kind
import Yaifl.Preconditions
import Yaifl.Container.Query
import Yaifl.Supporter.Query
import Yaifl.Animal.Query
import qualified Data.Text as T
import Yaifl.Entity (EnclosingTag)
import Yaifl.Backdrop.Kind

data EnteringResponses wm =
    EnterAlreadyEnteredA
    | EnterAlreadyEnteredB
    | EnterNotEnterableA
    | EnterNotEnterableB
    | EnterNotEnterableC
    | EnterNotEnterableD
    | EnterClosedContainerA
    | EnterCapacityA
    | EnterCapacityB
    | EnterCarriedA
    | EnterPassBarriersA
    | EnterPassBarriersB
    | EnterPassBarriersC
    | EnterPassBarriersD
    | EnterPassBarriersE
    | EnterReportA
    | EnterReportB
    | EnterReportC
    | EnterReportD

type EnteringAction wm = Action wm (EnteringResponses wm) (TakesOneOf TakesDirectionParameter TakesThingParameter) (Thing wm)

type EnteringRule wm = ActionRule wm (EnteringAction wm) (Thing wm)

-- TODO: "supplying a missing noun rulebook"
{-
Rule for supplying a missing noun while entering (this is the find what to enter
rule):
    if something enterable (called the box) is in the location,
        now the noun is the box;
    otherwise continue the activity.

The find what to enter rule is listed last in the for supplying a missing noun
rulebook.
-}
enteringAction ::
  WithPrintingTheLocaleDescription wm
  => WMWithProperty wm MultiLocated
  => WMWithProperty wm Enterable
  => WMWithProperty wm Backdrop
  => WMWithProperty wm Door
  => EnteringAction wm
enteringAction = (makeAction "entering")
  { name = "entering"
  , understandAs = ["enter", "go in", "go into", "enter into", "get into", "get in", "get on", "sit in", "sit on"]
  , parseArguments = ParseArguments $ \(UnverifiedArgs Args{..}) -> do
      case fst variables of
        Left dir -> return $ ConversionTo "go" [DirectionParameter dir]
        Right t -> do
          let mbDoor = getDoorMaybe t
          case mbDoor of
            Just _door -> return $ ConversionTo "go" [ThingParameter t]
            Nothing -> return $ SuccessfulParse t
  , checkRules = makeActionRulebook "check entering rulebook"
    [ cantEnterWhenEntered
    , cantEnterUnenterable -- this one possibly needs to be moved to parse arguments too
    , cantEnterClosedContainers
    , cantExceedCapacity
    , notImplementedRule "cant enter carried things"
    , implicitlyPassThrough
    ]
  , carryOutRules = makeActionRulebook "carry out entering rulebook" [ standardEntering ]
  , reportRules = makeActionRulebook "report entering rulebook"
    [ standardReportEntering
    , describeEntered
    ]
  }

cantEnterWhenEntered :: WMWithProperty wm MultiLocated => WMWithProperty wm Backdrop
  => SayableValue (WMText wm) wm => EnteringRule wm
cantEnterWhenEntered = makeRule "can't enter what's already entered rule" [] $ \a@Args{source=s, variables=v} -> withActionInterrupt' $ do
  -- if the actor is the noun, make no decision;
  when (v `objectEquals` s) $ throwError ContinueAction
  localCeiling <- getLocalCeiling s v
  -- if the local ceiling is the noun:
  when (localCeiling `objectEquals` v) $ do
    -- if the player is the actor:
      whenPlayer s $ do
        --  if the noun is a supporter:
        ifM (isSupporter v)
          -- say "But [we]['re] already on [the noun]." (A);
          (sayResponse EnterAlreadyEnteredA a)
          -- say "But [we]['re] already in [the noun]." (B);
          (sayResponse EnterAlreadyEnteredB a)
        throwError StopAction
  rulePass

commandIncludes :: Text -> Args wm v -> Bool
commandIncludes s args = T.isInfixOf s (command args)

cantEnterUnenterable :: SayableValue (WMText wm) wm => WMWithProperty wm Enterable => EnteringRule wm
cantEnterUnenterable = makeRule "can't enter what's not enterable rule" [] $ \a@Args{source=s, variables=v} -> withActionInterrupt' $ do
  -- if the noun is not enterable:
  when (isNothing (getEnterableMaybe v)) $ do
    -- if the player is the actor:
    whenPlayer s $ do
      if
        | commandIncludes "stand" a ->
            -- say "[regarding the noun][They're] not something [we] [can] stand on." (A);
            sayResponse EnterNotEnterableA a
        | commandIncludes "sit" a ->
            -- say "[regarding the noun][They're] not something [we] [can] sit down on." (B);
            sayResponse EnterNotEnterableB a
        | commandIncludes "lie" a ->
            -- say "[regarding the noun][They're] not something [we] [can] lie down on." (C);
            sayResponse EnterNotEnterableC a
        | otherwise ->
            -- say "[regarding the noun][They're] not something [we] [can] enter." (D);
            sayResponse EnterNotEnterableD a
    throwError StopAction
  rulePass

getLocalCeiling :: WMWithProperty wm MultiLocated => WMWithProperty wm Backdrop
  => RuleEffects wm es => Thing wm -> Thing wm -> Eff es EnclosingEntity
getLocalCeiling s v = do
  let actorHolder = thingContainedBy s
      nounHolder = thingContainedBy v
  -- let the local ceiling be the common ancestor of the actor with the noun;
  if actorHolder == nounHolder
        then return actorHolder
        else getCommonAncestor s v

implicitlyPassThrough :: forall wm. (WMWithProperty wm MultiLocated, WithPrintingNameOfSomething wm) => WMWithProperty wm Backdrop
  => EnteringRule wm
implicitlyPassThrough = makeRule "can't enter closed containers rule" [] $ \a@Args{source=s, variables=v} -> withActionInterrupt' $ do
  let actorHolder = thingContainedBy s
      nounHolder = thingContainedBy v
  localCeiling <- getLocalCeiling s v
  -- if the holder of the actor is the holder of the noun, continue the action;
  when (actorHolder == nounHolder) $ throwError ContinueAction

  -- while the holder of the actor is not the local ceiling:
  whileM (\actor -> not $ thingContainedBy actor `objectEquals` localCeiling) $ do
    actor <- refreshThing s
    --let the current home be the holder of the actor;
    currentHome <- getObject $ thingContainedBy actor
    --  if the player is the actor:
    whenPlayer s $
      --  if the current home is a supporter or the current home is an animal:
      ifM (isSupporter currentHome ||^ isAnimal currentHome)
        -- say "(getting off [the current home])[command clarification break]" (A);
        [saying|(getting off {the currentHome})#{linebreak}|]
        -- otherwise:
        -- say "(getting out of [the current home])[command clarification break]" (B);
        [saying|(getting out of {the currentHome})#{linebreak}|]
    -- silently try the actor trying exiting;
    void $ parseAction ((actionOptions a) { silently = True }) [] "exit"
    actor' <- refreshThing s
    let actorHolder' = thingContainedBy actor'
    -- if the holder of the actor is the current home, stop the action;
    when (actorHolder' `objectEquals` currentHome) $ throwError StopAction
    -- if the holder of the actor is the noun, stop the action;
    when (actorHolder' `objectEquals` v) $ throwError StopAction
    -- if the holder of the actor is the holder of the noun, continue the action;
    when (actorHolder' `objectEquals` nounHolder) $ throwError ContinueAction
    return actor'

    -- TODO: if the noun is part of the target, let the target be the holder of the target;
    -- this whole mess is basically "just enter the other thing once and then start the action again"
    -- let the target be the holder of the noun;
    -- while the target is a thing:
  when (isRoom nounHolder) $ throwError ContinueAction
  mbTarget <- getThingMaybe nounHolder
  case mbTarget of
    Nothing -> throwError ContinueAction
    Just target -> do
      -- if the holder of the target is the local ceiling:
      when (thingContainedBy target `objectEquals` localCeiling) $ do
        -- if the player is the actor:
        --   if the target is a supporter:
        whenPlayer s $
          ifM
            (isSupporter target)
            -- say "(getting onto [the target])[command clarification break]" (C);
            [saying|(getting onto {the target})#{linebreak}|]
            (ifM (isContainer target)
            -- otherwise if the target is a container:
            -- say "(getting into [the target])[command clarification break]" (D);
              [saying|(getting into {the target})#{linebreak}|]
            -- otherwise:
            -- say "(entering [the target])[command clarification break]" (E);
              [saying|(entering {the target})#{linebreak}|]
            )
        -- silently try the actor trying entering the target;
        void $ parseAction ((actionOptions a) { silently = True }) [ThingParameter target] "enter"
        -- if the holder of the actor is not the target, stop the action;
        actor' <- refreshThing s
        let actorHolder' = thingContainedBy actor'
        unless (actorHolder' `objectEquals` target) $ throwError StopAction
        void $ parseAction ((actionOptions a) { silently = True }) [ThingParameter v] "enter"
  rulePass

cantEnterClosedContainers :: (WithPrintingNameOfSomething wm, WMWithProperty wm Container) => EnteringRule wm
cantEnterClosedContainers = makeRule "can't enter closed containers rule" [] $ \Args{source=s, variables=v} -> do
  let asC = getContainerMaybe v
  t <- getThing v
  --if the noun is a closed container:
  ruleWhen (isClosedContainer <$?> asC) $ do
    -- if the player is the actor:
    whenPlayer s [saying|#{We} #{can't get} into the closed {t}.|]
    return (Just False)

cantExceedCapacity :: EnteringRule wm
cantExceedCapacity = notImplementedRule "can't enter if this exceeds carrying capacity"

standardEntering :: forall wm. WMWithProperty wm Enclosing => EnteringRule wm
standardEntering = makeRule "standard entering" [] $ \a@Args{variables=v} -> do
  case (getEnclosingMaybe $ toAny v) of
    Nothing -> noteRuntimeError (const (Just False)) $ "Encountered a non-enclosing thing in the standard entering rules" <> show (display $ view #name v)
    Just e ->  bool (Just True) Nothing <$> move (source a) (tagObject @EnclosingTag e v)

standardReportEntering :: WithPrintingNameOfSomething wm => EnteringRule wm
standardReportEntering = makeRule "standard report entering" [] $ \a@Args{source=s, variables=v} -> do
  -- if the actor is the player:
  ifM (isPlayer (source a))
    ( -- if the action is not silent:
      unlessSilent a $
        --if the noun is a supporter:
        ifM (isSupporter v)
          -- say "[We] [get] onto [the noun]." (A);
            [saying|#{We} #{get} onto {the v}.|]
          -- otherwise:
          -- say "[We] [get] into [the noun]." (B);
            [saying|#{We} #{get} into {the v}.|]
    )
    (  -- otherwise if the noun is a container:
      ifM (isContainer v)
        -- say "[The actor] [get] into [the noun]." (C);
        [saying|{The s} #{get} into {the v}.|]
        -- otherwise:
        -- say "[The actor] [get] onto [the noun]." (D);
        [saying|{The s} #{get} onto {the v}.|]
    )
  rulePass

describeEntered ::
  WithPrintingTheLocaleDescription wm
  => EnteringRule wm
describeEntered = makeRule "describe contents entered into" forPlayer' $ \Args{variables=v} -> do
  doActivity #printingTheLocaleDescription (LocaleVariables emptyStore (toAny v) 0)
  rulePass
