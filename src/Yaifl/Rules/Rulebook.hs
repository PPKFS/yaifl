{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Yaifl.Rules.Rulebook
  ( -- * Types
    Args(..)
  , UnverifiedArgs(..)
  , Rulebook(..)
    -- * Helper functions
  , addRuleFirst
  , addRuleLast
  , blankRulebook
  ) where


import Solitude

import Yaifl.Rules.Args
import Yaifl.Rules.Rule

-- | A 'Rulebook' is a computation (ia -> m (Maybe r)) built out of an initialisation (ia -> Maybe v), a default `Maybe r`,
-- and component rules `[(Text, (v -> m (Maybe v, Maybe r))]`
data Rulebook wm v r = Rulebook
  { name :: Text
  , defaultOutcome :: Maybe r
  , rules :: [Rule wm v r]
  } deriving stock (Generic)

blankRulebook ::
  Text
  -> Rulebook wm v r
blankRulebook n = Rulebook n Nothing []

makeFieldLabelsNoPrefix ''Rulebook

-- | Add a rule to a rulebook last.
addRuleLast ::
  Rule wm v r
  -> Rulebook wm v r
  -> Rulebook wm v r
addRuleLast r = #rules %~ (++ [r])

-- | Add a rule to a rulebook first.
addRuleFirst ::
  Rule wm v r
  -> Rulebook wm v r
  -> Rulebook wm v r
addRuleFirst r = #rules %~ (r :)
