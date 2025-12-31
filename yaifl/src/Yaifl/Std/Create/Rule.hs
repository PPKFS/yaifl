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
  , tryAction
  , ParameterReference(..)
  --, doAction
  --, doAction
  ) where

import Yaifl.Prelude
import Yaifl.Actions.Imports
import Yaifl.Activity
import Yaifl.AnyObject
import Yaifl.Object.Kind
import Yaifl.Thing.Kind
import Yaifl.Metadata (isKind)
import Yaifl.ObjectLike
import Yaifl.Enclosing.Query
import Yaifl.Refreshable
import Yaifl.Effects.RuleEffects
import Yaifl.Actions.Collection (ActionCollection)

import Yaifl.Person.Kind
import Yaifl.Std.Rulebooks.ActionProcessing
import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.Property.Has
import Yaifl.MultiLocated.Kind

type ActionPointer wm resps goesWith v = (Lens' (ActionCollection wm) (Action wm resps goesWith v))
newtype ActionOrActivity wm resps goesWith v = ActionRule (ActionPointer wm resps goesWith v)
  deriving stock (Generic)

before ::
  State (ActionCollection wm) :> es
  => ActionPointer wm resps goesWith v
  -> [Precondition wm (Args wm v)]
  -> Text
  -> (forall es'. (RuleEffects wm es', Refreshable wm (Args wm v)) => Args wm v -> Eff es' (Maybe Bool)) -- ^ Rule function.
  -> Eff es ()
before a precs t f = do
  let rule = makeRule t precs f
  a % #beforeRules %= addRuleLast rule
  pass

after ::
  State (ActionCollection wm) :> es
  => ActionPointer wm resps goesWith v
  -> [Precondition wm (Args wm v)]
  -> Text
  -> (forall es'. (RuleEffects wm es', Refreshable wm (Args wm v)) => Args wm v -> Eff es' (Maybe Bool)) -- ^ Rule function.
  -> Eff es ()
after a precs t f = do
  let rule = makeRule t precs f
  a % #afterRules %= addRuleLast rule
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

data ParameterReference wm =
  TheDirection (WMDirection wm)
  | TheObject Entity
  | TheThing ThingEntity
  | TheConstant Text
  | TheList [ParameterReference wm]
  deriving stock ( Generic )

toNamedActionParameter ::
  WithoutMissingObjects wm es
  => ParameterReference wm
  -> Eff es (ActionParameter wm)
toNamedActionParameter = \case
  TheDirection dir -> return $ DirectionParameter dir
  TheObject e -> ObjectParameter <$> getObject e
  TheThing t -> ThingParameter <$> getThing t
  TheConstant t -> return $ ConstantParameter t
  TheList l -> PluralParameter <$> mapM toNamedActionParameter l

-- | Attempt to run an action from a text command (so will handle the parsing).
-- Note that this does require the arguments to be parsed out.
tryAction ::
  forall wm v es.
  RuleEffects wm es
  => Text
  -> [ParameterReference wm]
  -> Args wm v
  -> Eff es Bool
tryAction ac params args = do
  nap <- mapM toNamedActionParameter params
  either error (error . show) <$> parseAction ((actionOptions args) { silently = False }) nap ac

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
