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

data GettingOffResponses wm

type GettingOffAction wm = Action wm () 'TakesThingParameter (SupporterThing wm)

gettingOffAction :: (WithPrintingNameOfSomething wm, WMWithProperty wm Enclosing, WMWithProperty wm Container, WMWithProperty wm Supporter) => GettingOffAction wm
gettingOffAction = (makeAction "getting off")
  { name = "getting off"
  , understandAs = ["get off"]
  , matches = [("from", TakesThingParameter)]
  , parseArguments = ParseArguments $ \(UnverifiedArgs Args{..}) -> do
      let mbS = getSupporterMaybe (fst variables)
      case mbS of
        Nothing -> return $ FailedParse "can't get off a not-supporter"
        Just s -> return $ SuccessfulParse (tagObject s (fst variables))
          {-
          if the actor is on the noun, continue the action;
          if the actor is carried by the noun, continue the action;
          if the actor is the player:
              say "But [we] [aren't] on [the noun] at the f story tense is present
                  tense]moment[otherwise]time[end if]." (A);
          stop the action.
          -}
  , carryOutRules = makeActionRulebook "carry out gettingOff rulebook" [ standardGettingOff ]
  , reportRules = makeActionRulebook "report getting off rulebook"
    [ notImplementedRule "standard report getting off"
    , notImplementedRule "describe room stood up into"
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