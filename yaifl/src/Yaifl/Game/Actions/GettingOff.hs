{-# LANGUAGE RecordWildCards #-}
module Yaifl.Game.Actions.GettingOff where

import Yaifl.Model.Action
import Yaifl.Prelude
import Yaifl.Model.Actions.Args
import Yaifl.Model.Rules.Rulebook
import Yaifl.Text.Responses
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Kinds.Direction
import Yaifl.Model.WorldModel
import Yaifl.Text.Say
import Yaifl.Model.Kinds
import Yaifl.Model.HasProperty
import Yaifl.Model.Kinds.Enclosing
import Yaifl.Game.Move
import Yaifl.Model.Kinds.Container
import Yaifl.Model.Tag
import Yaifl.Model.Entity (EnclosingTag)
import Yaifl.Model.Query
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Metadata
import Yaifl.Model.Kinds.Supporter

data GettingOffResponses wm

type GettingOffAction wm = Action wm () ('TakesOneOf 'TakesObjectParameter 'TakesNoParameter) (EnclosingThing wm)

gettingOffAction :: (WithPrintingNameOfSomething wm, WMWithProperty wm Enclosing, WMWithProperty wm Container, WMWithProperty wm Supporter) => GettingOffAction wm
gettingOffAction = (makeAction "getting off")
  { name = "getting off"
  , understandAs = ["get off"]
  , matches = [("from", TakesObjectParameter)]
  , parseArguments = ParseArguments $ \(UnverifiedArgs a@Args{..}) -> do
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
                Just _ -> ConversionTo "get off " [ThingParameter t])
        (return $ ConversionTo "go out" []) outFrom
  , beforeRules = makeActionRulebook "before gettingOff rulebook" []
  , insteadRules = makeActionRulebook "instead of gettingOff rulebook" []
  , checkRules = makeActionRulebook "check gettingOff rulebook"
    [ convertExitGoing
    , cantExitNotInExitable
    , cantExitGetOff
    ]
  , carryOutRules = makeActionRulebook "carry out gettingOff rulebook" [ standardGettingOff ]
  , reportRules = makeActionRulebook "report gettingOff rulebook"
    [ notImplementedRule "standard report gettingOff"
    , notImplementedRule "describe room emerged into"
    ]
  }

type GettingOffRule wm = ActionRule wm (GettingOffAction wm) (EnclosingThing wm)