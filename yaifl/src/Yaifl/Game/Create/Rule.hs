module Yaifl.Game.Create.Rule
  ( before
  , after
  , insteadOf
  , theObject
  , whenIn
  , ActionOrActivity(..)
  ) where

import Yaifl.Prelude
import Data.Text.Display
import Effectful.Optics
import Yaifl.Model.Action
import Yaifl.Game.Actions.Collection (ActionCollection)
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Entity
import Yaifl.Model.ObjectLike
import Yaifl.Model.Query
import Yaifl.Model.Tag
import Yaifl.Model.Actions.Args
import Yaifl.Model.Rules.Rulebook
import Yaifl.Model.Rules.RuleEffects
import Yaifl.Model.Kinds.AnyObject

newtype ActionOrActivity wm resps goesWith v = ActionRule (Lens' (ActionCollection wm) (Action wm resps goesWith v))
  deriving stock (Generic)

before ::
  State (ActionCollection wm) :> es
  => ActionOrActivity wm resps goesWith v
  -> [Precondition wm (Args wm v)]
  -> Text
  -> (forall es'. (RuleEffects wm es', Refreshable wm (Args wm v)) => Args wm v -> Eff es' (Maybe Bool)) -- ^ Rule function.
  -> Eff es ()
before a precs t f = do
  let rule = makeRule t precs f
  case a of
    ActionRule an -> an % #beforeRules %= addRuleLast rule
  pass

after ::
  State (ActionCollection wm) :> es
  => ActionOrActivity wm resps goesWith v
  -> [Precondition wm (Args wm v)]
  -> Text
  -> (forall es'. (RuleEffects wm es', Refreshable wm (Args wm v)) => Args wm v -> Eff es' (Maybe Bool)) -- ^ Rule function.
  -> Eff es ()
after a precs t f = do
  let rule = makeRule t precs f
  case a of
    ActionRule an -> an % #afterRules %= addRuleLast rule
  pass

insteadOf ::
  State (ActionCollection wm) :> es
  => ActionOrActivity wm resps goesWith v
  -> [Precondition wm (Args wm v)]
  -> (forall es'. (RuleEffects wm es', Refreshable wm (Args wm v)) => Args wm v -> Eff es' a) -- ^ Rule function.
  -> Eff es ()
insteadOf a precs f = do
  let rule = makeRule "" precs (fmap (\v -> v >> pure (Just True)) f)
  case a of
    ActionRule an -> an % #insteadRules %= addRuleLast rule
  pass

theObject ::
  ArgsMightHaveMainObject v (AnyObject wm)
  => ObjectLike wm o
  => o
  -> Precondition wm (Args wm v)
theObject o = Precondition
  { preconditionName = do
      e <- getObject o
      pure $ "to the object " <> display (e ^. #name)
  , checkPrecondition = \args -> do
      o' <- getObject o
      pure $ args ^? #variables % argsMainObjectMaybe == Just o'
  }

whenIn ::
  TaggedAs e EnclosingTag
  => ObjectLike wm e
  => e
  -> Precondition wm (Args wm v)
whenIn e = Precondition
  { preconditionName = do
      e' <- getObject e
      pure $ "when in the location " <> display (e' ^. #name)
  , checkPrecondition = \args -> do
      hierarchy <- getContainingHierarchy (args ^. #source)
      pure $ elem (toTag e) hierarchy
  }