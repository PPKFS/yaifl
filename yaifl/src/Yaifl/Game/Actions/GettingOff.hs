{-# LANGUAGE RecordWildCards #-}
module Yaifl.Game.Actions.GettingOff where

import Yaifl.Model.Action
import Yaifl.Prelude
import Yaifl.Model.Actions.Args
import Yaifl.Model.Rules.Rulebook
import Yaifl.Text.Say
import Yaifl.Model.Kinds.Thing ( EnclosingThing, thingContainedBy )
import Yaifl.Model.HasProperty
import Yaifl.Model.Kinds.Enclosing
import Yaifl.Model.Kinds.Container
import Yaifl.Model.Tag
import Yaifl.Model.Kinds.Supporter
import Yaifl.Game.Move (move)
import Yaifl.Model.Query
import Yaifl.Model.Entity
import Yaifl.Model.Kinds.AnyObject
import Breadcrumbs

data GettingOffResponses wm

type GettingOffAction wm = Action wm () ('TakesOneOf 'TakesThingParameter 'TakesNoParameter) (SupporterThing wm)

gettingOffAction :: (WithPrintingNameOfSomething wm, WMWithProperty wm Enclosing, WMWithProperty wm Container, WMWithProperty wm Supporter) => GettingOffAction wm
gettingOffAction = (makeAction "getting off")
  { name = "getting off"
  , understandAs = ["get off"]
  , matches = [("from", TakesThingParameter)]
  , parseArguments = ParseArguments $ \(UnverifiedArgs Args{..}) -> do
      offFrom <- case fst variables of
          Left thingToExit -> return (Just thingToExit)
          Right _ -> getThingMaybe $ thingContainedBy source
      let mbS = getSupporterMaybe =<< offFrom
      case (offFrom, mbS) of
        (Just t, Just s) -> return $ SuccessfulParse (tagObject s t)
          {-
          if the actor is on the noun, continue the action;
          if the actor is carried by the noun, continue the action;
          if the actor is the player:
              say "But [we] [aren't] on [the noun] at the f story tense is present
                  tense]moment[otherwise]time[end if]." (A);
          stop the action.
          -}
        _ -> return $ FailedParse "can't get off a not-supporter"
  , carryOutRules = makeActionRulebook "carry out getting off rulebook" [ standardGettingOff ]
  , reportRules = makeActionRulebook "report getting off rulebook"
    [ reportGettingOff
    , describeExited
    ]
  }

type GettingOffRule wm = ActionRule wm (GettingOffAction wm) (SupporterThing wm)

standardGettingOff ::
  WMWithProperty wm Enclosing
  => GettingOffRule wm
standardGettingOff = makeRule "standard getting off rule" [] $ \Args{source=s, variables=v} -> do
  let supporterHolder = thingContainedBy (getTaggedObject v)
  e' <- getEnclosingObject supporterHolder
  move s (tagObject @_ @EnclosingTag (snd e') (fst e'))
  rulePass

reportGettingOff ::
  WithPrintingNameOfSomething wm
  => GettingOffRule wm
reportGettingOff = makeRule "standard report getting off rule" [] $ \a@Args{source=s, variables=v} -> do
  -- if the action is not silent:
  unlessSilent a
    -- say "[The actor] [get] off [the noun]." (A);
    [saying|{The s} #{get} off {the v}.|]
  rulePass

describeExited ::
  GettingOffRule wm
describeExited = makeRule "describe room stood up into rule" forPlayer' $ \a@Args{variables=v} -> do
  -- TODO: reckon darkness
  parseAction ((actionOptions a) { silently = True }) [ConstantParameter "going"] "look"
  rulePass