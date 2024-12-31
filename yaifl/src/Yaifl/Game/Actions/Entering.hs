{-# LANGUAGE RecordWildCards #-}
module Yaifl.Game.Actions.Entering where

import Yaifl.Model.Action
import Yaifl.Prelude
import Yaifl.Core.Actions.Args
import Yaifl.Model.Rules.Rulebook
import Yaifl.Text.Say
import Yaifl.Core.Kinds.Enclosing
import Yaifl.Game.Move
import Yaifl.Model.Kinds.Container
import Yaifl.Core.Tag
import Yaifl.Core.Query.Object
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Core.Metadata
import Yaifl.Model.Kinds.Supporter
import Yaifl.Model.Kinds.Animal
import Effectful.Error.Static
import Yaifl.Model.Activity
import Yaifl.Core.Store
import Yaifl.Game.Actions.Looking.Locale
import Yaifl.Game.Activities.PrintingTheLocaleDescription (WithPrintingTheLocaleDescription)
import Yaifl.Core.WorldModel
import Yaifl.Core.ObjectLike
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Refreshable
import Yaifl.Core.Kinds.Object
import Yaifl.Core.Query.Enclosing
import Yaifl.Core.Actions.GoesWith

data EnteringResponses wm

type EnteringAction wm = Action wm () 'TakesThingParameter (EnclosingThing wm)

-- TODO: "supplying a missing noun rulebook"

enteringAction ::
  WithPrintingTheLocaleDescription wm
  => EnteringAction wm
enteringAction = (makeAction "entering")
  { name = "entering"
  , understandAs = ["enter", "go in", "go into", "enter into", "get into", "get in", "get on", "sit in", "sit on"]
  , parseArguments = ParseArguments $ \(UnverifiedArgs Args{..}) -> do
      let mbCont = getEnclosingMaybe (toAny $ fst variables)
      case mbCont of
        Nothing -> return $ FailedParse "that's not enterable"
        Just x -> return $ SuccessfulParse (tagObject x (fst variables))
  , beforeRules = makeActionRulebook "before entering rulebook" []
  , insteadRules = makeActionRulebook "instead of entering rulebook" []
  , checkRules = makeActionRulebook "check entering rulebook"
    [ convertEnterDoor
    , convertEnterDirection -- this one won't work - it needs to be an interpret as, and it needs to be before we parse..
    , cantEnterWhenEntered
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

type EnteringRule wm = ActionRule wm (EnteringAction wm) (EnclosingThing wm)

convertEnterDoor :: EnteringRule wm
convertEnterDoor = notImplementedRule "convert enter door"

convertEnterDirection :: EnteringRule wm
convertEnterDirection = notImplementedRule "convert enter direction"

cantEnterWhenEntered :: EnteringRule wm
cantEnterWhenEntered = notImplementedRule "cant enter what's already entered"

cantEnterUnenterable :: EnteringRule wm
cantEnterUnenterable = notImplementedRule "can't enter what's not enterable rule"

implicitlyPassThrough :: forall wm. WithPrintingNameOfSomething wm => EnteringRule wm
implicitlyPassThrough = makeRule "can't enter closed containers rule" [] $ \a@Args{source=s, variables=v} -> withActionInterrupt' $ do
  let actorHolder = thingContainedBy s
      nounHolder = thingContainedBy $ getTaggedObject v
  -- let the local ceiling be the common ancestor of the actor with the noun;
  localCeiling <-
        if actorHolder == nounHolder
        then return actorHolder
        else
          do
            acHier <- getContainingHierarchy s
            nounHier <- getContainingHierarchy (getTaggedObject v)
            -- we can cheat doing a proper lowest common ancestor. we can take one of the hierarchies
            -- (which one is irrelevant), and find the earliest possible match in the other list
            let commAncestor (l1h :| l1s) l2 = if l1h `elem` l2 then l1h else commAncestor
                  (case l1s of
                    [] -> error "no common ancestor"
                    x:xs -> x :| xs) l2
            return $ commAncestor acHier nounHier
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
        void $ parseAction ((actionOptions a) { silently = True }) [ThingParameter (getTaggedObject v)] "enter"
  rulePass

cantEnterClosedContainers :: (WithPrintingNameOfSomething wm, WMWithProperty wm Container) => EnteringRule wm
cantEnterClosedContainers = makeRule "can't enter closed containers rule" [] $ \Args{source=s, variables=v} -> do
  let asC = getContainerMaybe (getTaggedObject v)
  t <- getThing v
  --if the noun is a closed container:
  ruleWhen (isClosedContainer <$?> asC) $ do
    -- if the player is the actor:
    whenPlayer s $
      [saying|#{We} #{can't get} into the closed {t}.|]
    return (Just False)


cantExceedCapacity :: EnteringRule wm
cantExceedCapacity = notImplementedRule "can't enter if this exceeds carrying capacity"

standardEntering :: WMWithProperty wm Enclosing => EnteringRule wm
standardEntering = makeRule "standard entering" [] $ \a@Args{variables=v} ->
  bool (Just True) Nothing <$> move (source a) v

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
  doActivity #printingTheLocaleDescription (LocaleVariables emptyStore (toAny $ getTaggedObject v) 0)
  rulePass
