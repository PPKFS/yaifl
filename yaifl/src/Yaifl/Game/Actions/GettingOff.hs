{-# LANGUAGE RecordWildCards #-}
module Yaifl.Game.Actions.GettingOff where

import Yaifl.Model.Action
import Yaifl.Prelude
import Yaifl.Core.Actions.Args
import Yaifl.Model.Rules.Rulebook
import Yaifl.Text.Say
import Yaifl.Core.Kinds.Thing ( thingContainedBy )
import Yaifl.Core.Kinds.Enclosing
import Yaifl.Core.Tag ( getTaggedObject, tagObject )
import Yaifl.Model.Kinds.Supporter
import Yaifl.Game.Move (move)
import Yaifl.Core.Query.Object
import Yaifl.Core.Metadata
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.Verb (Tense(..))
import Yaifl.Core.WorldModel
import Yaifl.Core.Query.Enclosing
import Yaifl.Core.Actions.GoesWith

data GettingOffResponses wm

type GettingOffAction wm = Action wm () ('TakesOneOf 'TakesThingParameter 'TakesNoParameter) (SupporterThing wm)

gettingOffAction :: forall wm. (WithPrintingNameOfSomething wm, WMWithProperty wm Enclosing, WMWithProperty wm Supporter) => GettingOffAction wm
gettingOffAction = (makeAction "getting off")
  { name = "getting off"
  , understandAs = ["get off", "get up"]
  , matches = [("from", TakesThingParameter)]
  , parseArguments = ParseArguments $ \(UnverifiedArgs Args{..}) -> do
      offFrom <- case fst variables of
          Left thingToExit -> return (Just thingToExit)
          Right _ -> getThingMaybe $ thingContainedBy source
      let mbS = getSupporterMaybe =<< offFrom
      present <- (Present ==) <$> use @(AdaptiveNarrative wm) #tense
      case (offFrom, mbS) of
        (Just t, Just s) -> return $ SuccessfulParse (tagObject s t)
          {-
          if the actor is on the noun, continue the action;
          if the actor is carried by the noun, continue the action;
          stop the action.
          -}
        (Just something, Nothing) -> do
          -- if the actor is the player:
          -- say "But [we] [aren't] on [the noun] at the f story tense is present
          -- tense]moment[otherwise]time[end if]." (A);
          whenPlayer source
            [saying|But #{we} #{aren't} on {the something} at the {?if present}moment{?else}time{?end if}.|]
          return $ FailedParse "can't get off a not-supporter"
        (Nothing, _) -> do
          whenPlayer source
            [saying|But #{we} #{aren't} on anything at the {?if present}moment{?else}time{?end if}.|]
          return $ FailedParse "can't get off nothing"
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
  move s e'
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
describeExited = makeRule "describe room stood up into rule" forPlayer' $ \a@Args{} -> do
  -- TODO: reckon darkness
  parseAction ((actionOptions a) { silently = True }) [ConstantParameter "going"] "look"
  rulePass