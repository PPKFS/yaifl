{-# LANGUAGE RecordWildCards #-}
module Yaifl.Game.Actions.Entering where

import Yaifl.Model.Action
import Yaifl.Prelude
import Yaifl.Model.Actions.Args
import Yaifl.Model.Rules.Rulebook
import Yaifl.Text.Say
import Yaifl.Model.Kinds
import Yaifl.Model.HasProperty
import Yaifl.Model.Kinds.Enclosing
import Yaifl.Game.Move
import Yaifl.Model.Kinds.Container
import Yaifl.Model.Tag
import Yaifl.Model.Query
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Metadata
import Yaifl.Model.Entity
import Yaifl.Model.Kinds.Supporter
import Yaifl.Model.Kinds.Animal
import Effectful.Error.Static

data EnteringResponses wm

type EnteringAction wm = Action wm () 'TakesThingParameter (EnclosingThing wm)

-- TODO: "supplying a missing noun rulebook"

enteringAction :: (WithPrintingNameOfSomething wm, WMWithProperty wm Enclosing, WMWithProperty wm Container) => EnteringAction wm
enteringAction = (makeAction "entering")
  { name = "entering"
  , understandAs = ["enter", "go in", "go into", "enter into", "get into", "get in", "get on"]
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
    [ notImplementedRule "standard report entering"
    , notImplementedRule "describe contents entered into"
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

implicitlyPassThrough :: WithPrintingNameOfSomething wm => EnteringRule wm
implicitlyPassThrough = makeRule "can't enter closed containers rule" [] $ \a@Args{source=s, variables=v} -> do
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
  whileM (error "") $ do
    actor <- refreshThing s
    --let the current home be the holder of the actor;
    currentHome <- getObject $ thingContainedBy actor
    --  if the player is the actor:
    whenPlayer s $
      --  if the current home is a supporter or the current home is an animal:
      ifM (isSupporter currentHome ||^ isAnimal currentHome)
        -- say "(getting off [the current home])[command clarification break]" (A);
        [saying|(getting off {the currentHome}#{paragraphBreak})|]
        -- otherwise:
        -- say "(getting out of [the current home])[command clarification break]" (B);
        [saying|(getting out of {the currentHome}#{paragraphBreak})|]
    -- silently try the actor trying exiting;
    void $ parseAction ((actionOptions a) { silently = True }) [] "exit"
    actor' <- refreshThing s
    let actorHolder' = thingContainedBy actor'
    -- if the holder of the actor is the current home, stop the action;
    when (actorHolder' `objectEquals` currentHome) $ throwError StopAction
    -- if the holder of the actor is the noun, stop the action;
    when (actorHolder' `objectEquals` v) $ throwError StopAction
    -- if the holder of the actor is the holder of the noun, continue the action;
    when (actorHolder' `objectEquals` nounHolder) $ throwError StopAction


  {-

    let the target be the holder of the noun;
    if the noun is part of the target, let the target be the holder of the target;
    while the target is a thing:
        if the holder of the target is the local ceiling:
            if the player is the actor:
                if the target is a supporter:
                    say "(getting onto [the target])[command clarification break]" (C);
                otherwise if the target is a container:
                    say "(getting into [the target])[command clarification break]" (D);
                otherwise:
                    say "(entering [the target])[command clarification break]" (E);
            silently try the actor trying entering the target;
            if the holder of the actor is not the target, stop the action;
            convert to the entering action on the noun;
            continue the action;
        let the target be the holder of the target;
  -}
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
standardEntering = makeRule "standard entering" [] $ \a@Args{variables=v} -> bool (Just True) Nothing <$> move (source a) v
