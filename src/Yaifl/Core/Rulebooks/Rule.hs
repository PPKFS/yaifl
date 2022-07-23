-- ~\~ language=Haskell filename=src/Yaifl/Core/Rulebooks/Rule.hs
-- ~\~ begin <<lit/rulebooks/rules-rulebooks.md|src/Yaifl/Core/Rulebooks/Rule.hs>>[0] project://lit/rulebooks/rules-rulebooks.md:4
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Yaifl.Core.Rulebooks.Rule 
  ( RuleEffects
  , Rule(..)
  , RuleCondition(..)
  , ActionOptions(..)
  , ActionHandler(..)
  , parseAction
  , notImplementedRule
  , makeRule
  , makeRule'
  , rulePass
  , ruleCondition
  , ruleCondition'

  , ruleName
  , runRule

  ) where

import Cleff.State
import Yaifl.Core.Common

import Yaifl.Core.Objects.Query
import Yaifl.Core.Logger hiding ( Error )
import Yaifl.Core.Say
import Yaifl.Core.Rulebooks.Args
import {-# SOURCE #-} Yaifl.Core.Actions.Activity
import Text.Interpolation.Nyan
import Cleff.Error (Error, throwError)
import Yaifl.Core.Objects.Object

data ActionHandler wm :: Effect where
  ParseAction :: ActionOptions wm -> Text -> ActionHandler wm m (Either Text Bool)

data ActionOptions wm = ActionOptions  
  { silently :: Bool
  , actor :: Maybe (Thing wm)

  }
makeEffect ''ActionHandler

data RuleCondition = RuleCondition 

type RuleEffects wm es = (
  State (Metadata) :> es
  , Log :> es
  , NoMissingObjects wm es
  , Saying :> es
  , ActionHandler wm :> es
  , ObjectTraverse wm :> es
  , State (ActivityCollection wm) :> es
  )

-- | A 'Rule' is a wrapped function with a name, that modifies the world (potentially)
-- and any rulebook variables, and might return an outcome (Just) or not (Nothing).
data Rule wm v r = Rule
  { _ruleName :: Text
  , _runRule :: forall es. (RuleEffects wm es, Error RuleCondition :> es, Refreshable wm v) => v -> Eff es (Maybe v, Maybe r)
  }

-- | A helper for rules which are not implemented and therefore blank.
notImplementedRule ::
  Text
  -> Rule wm v r
notImplementedRule n = makeRule' n (warn [int|t| #{n} needs implementing|] >> return Nothing)

-- | Make a rule that does not modify the action arguments.
makeRule :: 
  Text -- ^ Rule name.
  -> (forall es. (RuleEffects wm es, Error RuleCondition :> es, Refreshable wm v) => v -> Eff es (Maybe r)) -- ^ Rule function.
  -> Rule wm v r
makeRule n f = Rule n (fmap (Nothing, ) . f)

-- | Make a rule that does has no arguments. this is more convenient to avoid \() ->...
makeRule' :: 
  Text -- ^ Rule name.
  -> (forall es. (RuleEffects wm es, Error RuleCondition :> es) => Eff es (Maybe r)) -- ^ Rule function.
  -> Rule wm v r
makeRule' n f = makeRule n (const f)

-- | Remove any unwanted return values from a `Rule`.
rulePass :: 
  Monad m
  => m (Maybe a)
rulePass = return Nothing

makeLenses ''Rule

ruleCondition' :: 
  Error RuleCondition :> es
  => Eff es Bool
  -> Eff es ()
ruleCondition' f = do
  mbC <- f
  if mbC then pass else throwError RuleCondition

ruleCondition :: 
  Error RuleCondition :> es
  => Eff es (Maybe a)
  -> Eff es a
ruleCondition f = do
  mbC <- f
  case mbC of
    Nothing -> throwError RuleCondition
    Just r -> return r
-- ~\~ end
