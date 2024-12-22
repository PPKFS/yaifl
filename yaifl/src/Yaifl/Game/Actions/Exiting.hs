{-# LANGUAGE RecordWildCards #-}
module Yaifl.Game.Actions.Exiting where

import Yaifl.Model.Action
import Yaifl.Prelude
import Yaifl.Model.Actions.Args
import Yaifl.Model.Rules.Rulebook
import Yaifl.Text.Say
import Yaifl.Model.Kinds
import Yaifl.Model.HasProperty
import Yaifl.Model.Kinds.Enclosing
import Yaifl.Model.Kinds.Container
import Yaifl.Model.Tag
import Yaifl.Model.Query
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Metadata
import Yaifl.Model.Kinds.Supporter
import Breadcrumbs

data ExitingResponses wm

type ExitingAction wm = Action wm () ('TakesOneOf 'TakesObjectParameter 'TakesNoParameter) (EnclosingThing wm)

exitingAction :: (WithPrintingNameOfSomething wm, WMWithProperty wm Container, WMWithProperty wm Enclosing, WMWithProperty wm Supporter) => ExitingAction wm
exitingAction = (makeAction "exiting")
  { name = "exiting"
  , understandAs = ["exit", "get out", "out"]
  , matches = [("from", TakesObjectParameter)]
  , parseArguments = ParseArguments $ \(UnverifiedArgs Args{..}) -> do
      outFrom <- case fst variables of
          Left thingToExit -> return (toAny thingToExit)
          Right _ -> getObject $ thingContainedBy source
      return $ asThingOrRoom
        (\t ->
          case getEnclosingMaybe (toAny t) of
            Nothing -> FailedParse "that's not exitable"
            Just x ->
              case getSupporterMaybe t of
                Nothing -> SuccessfulParse (tagObject x t)
                -- this will convert to the get off action (because it can't match exiting again)
                Just _ -> ConversionTo "get off " [ThingParameter t])
        (return $ ConversionTo "go out" []) outFrom
  , checkRules = makeActionRulebook "check exiting rulebook"
    [ convertExitGoing
    , cantExitNotInExitable
    , cantExitClosedContainers
    , cantExitGetOff
    ]
  , carryOutRules = makeActionRulebook "carry out exiting rulebook" [ standardExiting ]
  , reportRules = makeActionRulebook "report exiting rulebook"
    [ notImplementedRule "standard report exiting"
    , notImplementedRule "describe room emerged into"
    ]
  }

type ExitingRule wm = ActionRule wm (ExitingAction wm) (EnclosingThing wm)

convertExitGoing :: ExitingRule wm
convertExitGoing = notImplementedRule "convert exit to going"

cantExitGetOff :: ExitingRule wm
cantExitGetOff = notImplementedRule "convert exit direction"

cantExitNotInExitable :: ExitingRule wm
cantExitNotInExitable = notImplementedRule "can't exit what's not exitable rule"

cantExitClosedContainers :: (WithPrintingNameOfSomething wm, WMWithProperty wm Container) => ExitingRule wm
cantExitClosedContainers = makeRule "can't exit closed containers rule" [] $ \Args{source=s, variables=v} -> do
  let asC = getContainerMaybe (getTaggedObject v)
  t <- getThing v
  addAnnotation $ display t <> show asC
  --if the noun is a closed container:
  ruleWhen (isClosedContainer <$?> asC) $ do
    -- if the player is the actor:
    whenPlayer s
      [saying|#{We} #{can't get} out of the closed {t}.|]
    return (Just False)


cantExceedCapacity :: ExitingRule wm
cantExceedCapacity = notImplementedRule "can't exit if this exceeds carrying capacity"

standardExiting :: WMWithProperty wm Enclosing => ExitingRule wm
standardExiting = makeRule "standard exiting" [] $ \a@Args{variables=v} -> rulePass

describeExited ::
  ExitingRule wm
describeExited = makeRule "describe contents entered into" forPlayer' $ \a@Args{variables=v} -> do
  -- TODO: reckon darkness
  parseAction ((actionOptions a) { silently = True }) [ConstantParameter "going"] "look"
  rulePass
