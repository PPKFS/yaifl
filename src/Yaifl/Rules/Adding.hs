module Yaifl.Rules.Adding
  ( before
  , ActionOrActivity(..)

  ) where

import Solitude
import Yaifl.Rules.RuleEffects
import Yaifl.Rules.Args
import Yaifl.Rules.Rule
import Yaifl.Actions.Action
import Yaifl.Rules.Rulebook (addRuleLast)
import Effectful.Optics
import Yaifl.Actions.Collection (ActionCollection)

newtype ActionOrActivity wm goesWith v = ActionRule (Lens' (ActionCollection wm) (Action wm goesWith v))
  deriving stock (Generic)

before ::
  State (ActionCollection wm) :> es
  => ActionOrActivity wm goesWith v
  -> [Precondition wm (Args wm v)]
  -> Text
  -> (forall es'. (RuleEffects wm es', Refreshable wm (Args wm v)) => Args wm v -> Eff es' (Maybe Bool)) -- ^ Rule function.
  -> Eff es ()
before a precs t f = do
  let rule = makeRule t precs f
  case a of
    ActionRule an -> an % #beforeRules %= addRuleLast rule
  pass