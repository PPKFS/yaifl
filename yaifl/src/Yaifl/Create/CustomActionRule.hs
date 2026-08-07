module Yaifl.Create.CustomActionRule
  ( before
  , before'
  , after
  , after'
  , insteadOf
  , insteadOf'
  ) where

import Yaifl.Prelude
import Yaifl.Actions.Imports
import Yaifl.Refreshable
import Yaifl.Effects.RuleEffects


type ActionPointer wm resps goesWith v = (Lens' (WMActions wm) (Action wm resps goesWith v))

type NewRule wm v = (forall es'. (RuleEffects wm es', Refreshable wm (Args wm v)) => Args wm v -> Eff es' (Maybe Bool))
type NewRule' wm = (forall es'. (RuleEffects wm es') => Eff es' (Maybe Bool))

before ::
  State (ActionCollector wm) :> es
  => ActionPointer wm resps goesWith v
  -> [Precondition wm (Args wm v)]
  -> Text
  -> NewRule wm v -- ^ Rule function.
  -> Eff es ()
before a precs t f = do
  let rule = makeRule t precs f
  #actionCollection % a % #beforeRules %= addRuleLast rule
  pass

before' ::
  State (ActionCollector wm) :> es
  => ActionPointer wm resps goesWith v
  -> [Precondition wm (Args wm v)]
  -> Text
  -> NewRule' wm
  -> Eff es ()
before' a precs t f = before a precs t (const f)

after ::
  State (ActionCollector wm) :> es
  => ActionPointer wm resps goesWith v
  -> [Precondition wm (Args wm v)]
  -> Text
  -> NewRule wm v
  -> Eff es ()
after a precs t f = do
  let rule = makeRule t precs f
  #actionCollection % a % #afterRules %= addRuleLast rule
  pass

after' ::
  State (ActionCollector wm) :> es
  => ActionPointer wm resps goesWith v
  -> [Precondition wm (Args wm v)]
  -> Text
  -> NewRule' wm
  -> Eff es ()
after' a precs t f = before a precs t (const f)

insteadOf ::
  State (ActionCollector wm) :> es
  => ActionPointer wm resps goesWith v
  -> [Precondition wm (Args wm v)]
  -> (forall es'. (RuleEffects wm es', Refreshable wm (Args wm v)) => Args wm v -> Eff es' a) -- ^ Rule function.
  -> Eff es ()
insteadOf a precs f = do
  let rule = makeRule "" precs (fmap (\v -> v >> pure (Just True)) f)
  #actionCollection % a % #insteadRules %= addRuleLast rule

insteadOf' ::
  State (ActionCollector wm) :> es
  => ActionPointer wm resps goesWith v
  -> [Precondition wm (Args wm v)]
  -> (forall es'. (RuleEffects wm es') => Eff es' a) -- ^ Rule function.
  -> Eff es ()
insteadOf' a precs f = insteadOf a precs (const f)

