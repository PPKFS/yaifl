module Yaifl.Gen.Plan
  ( Plannable(..)
  , Weight(..)
  , PlanOption
  , Options
  , Options2
  , Options3
  , Options4
  , SequentialOptions
  , SequentialOptions2
  , SequentialOptions3

  , pickOne
  , pickSequential

  , beforePlanWith
  , equalWeights

  ) where

import Yaifl.Prelude

class Plannable plan where
  type PlanM plan :: Type -> Type
  type PlanInput plan
  type PlanOutput plan
  runPlan :: Monad (PlanM plan) => plan -> PlanInput plan -> (PlanM plan) (PlanOutput plan)

newtype Weight = Weight Int
type PlanOption es a b = (a -> Eff es b)
type Options es a b = NonEmpty (Weight, PlanOption es a b)
type Options2 es a b c = Options es (a, b) c
type Options3 es a b c d = Options es (a, b, c) d
type Options4 es a b c d e = Options es (a, b, c, d) e
type SequentialOptions es a b = Options es (Int, a, [b]) b
type SequentialOptions2 es a b c = SequentialOptions es (a, b) c
type SequentialOptions3 es a b c d = SequentialOptions es (a, b, c) d

pickSequential :: Int -> a -> SequentialOptions es a d -> Eff es [d]
pickSequential n t opts = foldlM (\ds i -> (:ds) <$> pickOne opts (i, t, ds)) [] [0..n]

pickOne :: Options es a b -> a -> Eff es b
pickOne (x:|_opts) i = x ^. _2 $ i

beforePlanWith :: (a -> Eff es c) -> NonEmpty (Weight, PlanOption es (c, a) b) -> NonEmpty (Weight, PlanOption es a b)
beforePlanWith bef = fmap (second (\f -> \a -> bef a >>= \b -> f (b, a)))

equalWeights :: NonEmpty (PlanOption es a b) -> NonEmpty (Weight, PlanOption es a b)
equalWeights = fmap (Weight 1,)