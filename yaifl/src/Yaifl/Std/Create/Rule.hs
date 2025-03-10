module Yaifl.Std.Create.Rule
  ( before
  , after
  , insteadOf
  , theObject
  , whenIn
  , afterActivity
  , theObject'
  , afterPrintingTheNameOf
  , aKindOf
  , duringActivity
  , everyTurn
  , whenPlayerIsIn
  , ActionOrActivity(..)
  , ActionPointer
  , doAction
  --, doAction'
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Activity
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Core.Kinds.Object
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Metadata (isKind)
import Yaifl.Core.ObjectLike
import Yaifl.Core.Query.Enclosing
import Yaifl.Core.Refreshable
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Std.Actions.Collection (ActionCollection)

import Yaifl.Std.Kinds.Person
import Yaifl.Std.Rulebooks.ActionProcessing
import Breadcrumbs

type ActionPointer wm resps goesWith v = (Lens' (ActionCollection wm) (Action wm resps goesWith v))
newtype ActionOrActivity wm resps goesWith v = ActionRule (ActionPointer wm resps goesWith v)
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

afterActivity ::
  State (ActivityCollector wm) :> es
  => ActivityLens wm resps v r
  -> [Precondition wm v]
  -> Text
  -> (forall es'. (RuleEffects wm es', Refreshable wm v) => v -> Eff es' (Maybe r)) -- ^ Rule function.
  -> Eff es ()
afterActivity a precs t f = do
  let rule = makeRule t precs f
  #activityCollection % a % afterActivityRules %= addRuleLast rule
  pass

insteadOf ::
  State (ActionCollection wm) :> es
  => ActionPointer wm resps goesWith v
  -> [Precondition wm (Args wm v)]
  -> (forall es'. (RuleEffects wm es', Refreshable wm (Args wm v)) => Args wm v -> Eff es' a) -- ^ Rule function.
  -> Eff es ()
insteadOf a precs f = do
  let rule = makeRule "" precs (fmap (\v -> v >> pure (Just True)) f)
  a % #insteadRules %= addRuleLast rule

doAction ::
  forall wm goesWith es v'.
  GoesWith goesWith
  => RuleEffects wm es
  => Text
  -> (Args wm v' -> Eff es (UnverifiedArgs wm goesWith))
  -> Args wm v'
  -> Eff es (Maybe Bool)
doAction ac mapVars args = do
  newArgs <- mapVars args
  performAction (Proxy @goesWith) (actionOptions args) ac newArgs
{-}
doAction' ::
  forall goesWith wm v es.
  GoesWith goesWith
  => RuleEffects wm es
  => Refreshable wm v
  => Display v
  => Text
  -> Args wm v
  -> Eff es (Maybe Bool)
doAction' acp args = doAction acp (\a -> return  (UnverifiedArgs a)) args
-}
theObject ::
  ArgsMightHaveMainObject v (Thing wm)
  => ThingLike wm o
  => o
  -> Precondition wm (Args wm v)
theObject o = Precondition
  { preconditionName = do
      e <- getThing o
      pure $ "to the object " <> display (e ^. #name)
  , checkPrecondition = \args -> do
      o' <- getThing o
      pure $ args ^? #variables % argsMainObjectMaybe == Just o'
  }

theObject' ::
  ThingLike wm o
  => o
  -> Precondition wm (AnyObject wm)
theObject' o = Precondition
  { preconditionName = do
      e <- getThing o
      pure $ "to the object " <> display (e ^. #name)
  , checkPrecondition = \args -> do
      o' <- getThing o
      pure $ args `objectEquals` o'
  }

whenIn ::
  ObjectLike wm e
  => IsEnclosing e
  => e
  -> Precondition wm (Args wm v)
whenIn e = Precondition
  { preconditionName = do
      e' <- getObject e
      pure $ "when in the location " <> display (e' ^. #name)
  , checkPrecondition = \args -> do
      hierarchy <- getContainingHierarchy (args ^. #source)
      pure $ elem (getEnclosingEntity e) hierarchy
  }

whenPlayerIsIn ::
  ObjectLike wm e
  => IsEnclosing e
  => e
  -> Precondition wm a
whenPlayerIsIn e = Precondition
  { preconditionName = do
      e' <- getObject e
      pure $ "when in the location " <> display (e' ^. #name)
  , checkPrecondition = const $ do
      hierarchy <- getPlayer' >>= getContainingHierarchy
      pure $ elem (getEnclosingEntity e) hierarchy
  }

aKindOf ::
  ObjectKind
  -> Precondition wm (AnyObject wm)
aKindOf k@(ObjectKind kName) = Precondition
  { preconditionName = return $ "is of kind " <> kName
  , checkPrecondition = \noun -> do
      noun `isKind` k
  }

afterPrintingTheNameOf ::
  WithPrintingNameOfSomething wm
  => State (ActivityCollector wm) :> es
  => [Precondition wm (AnyObject wm)]
  -> Text
  -> (forall es'. (RuleEffects wm es', Refreshable wm (AnyObject wm)) => AnyObject wm -> Eff es' (Maybe Text)) -- ^ Rule function.
  -> Eff es ()
afterPrintingTheNameOf = afterActivity #printingNameOfSomething

everyTurn ::
  State (WorldActions wm) :> es
  => Text
  -> [Precondition wm ()]
  -> (forall es'. (RuleEffects wm es') => Eff es' ()) -- ^ Rule function.
  -> Eff es ()
everyTurn preReqs n r = #everyTurnRules %= addRuleLast (makeRule preReqs n (const $ r >> rulePass))

duringActivity ::
  forall wm resps v r a.
  ActivityLens wm resps v r
  -> Precondition wm a
duringActivity l = Precondition
  { preconditionName = do
      n <- use @(ActivityCollector wm) $ #activityCollection % l % #name
      return $ "during the activity " <> n
  , checkPrecondition = const $ isJust <$> (use @(ActivityCollector wm) $ #activityCollection % l % #currentVariables)
  }