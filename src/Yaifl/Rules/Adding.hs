module Yaifl.Rules.Adding
  (

  ) where

import Solitude
import Yaifl.Rules.RuleEffects
import Yaifl.Rules.Args
import Yaifl.Rules.Rule
import Yaifl.Actions.Action
import Yaifl.Rules.Rulebook (addRuleLast)
import Effectful.Optics

data ActionOrActivity = ActionRule Text | ActivityRule Text
  deriving stock (Eq, Show, Ord, Generic)
before ::
  State (WorldActions wm) :> es
  => ActionOrActivity
  -> [Precondition wm (Args wm v)]
  -> Text
  -> (forall es'. (RuleEffects wm es', Refreshable wm (Args wm v)) => Args wm v -> Eff es' (Maybe Bool)) -- ^ Rule function.
  -> Eff es ()
before a precs t f = do
  let rule = makeRule t precs f
  --let updateAction =  (\(WrappedAction wm) -> WrappedAction (wm & #beforeRules %~ addRuleLast rule ))
  case a of
    ActionRule an -> error "" -- #actionsMapL % at an %= error ""
    ActivityRule a -> error ""
  pass