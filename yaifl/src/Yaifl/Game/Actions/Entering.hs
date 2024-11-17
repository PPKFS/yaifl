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
import Yaifl.Model.Query (getEnclosingMaybe)
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Metadata

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
    , notImplementedRule "implicitly pass through other barriers"
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

cantEnterClosedContainers :: (WithPrintingNameOfSomething wm, WMWithProperty wm Container) => EnteringRule wm
cantEnterClosedContainers = makeRule "can't enter closed containers rule" [] $ \a@Args{source=s, variables=v} -> do
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
