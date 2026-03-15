{-|
Module      : Yaifl.Rulebooks.Accessibility
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Accessibility rulebook for determining object reachability.

This module provides rules for determining whether one object can access or reach
another object in the game world. It handles spatial relationships, container access,
and physical constraints that affect whether actions are possible.

The accessibility system is used by the action processing system to validate whether
proposed actions are physically feasible before attempting to execute them.

Key components:
- `accessibility`: Main rulebook containing accessibility rules
- `samePlace`: Rule checking if objects are in the same location
- `insideClosedContainers`: Rule checking container access constraints
-}

module Yaifl.Rulebooks.Accessibility
  ( -- * Main Rulebook
    accessibility

  -- * Accessibility Rules
  -- | Rule functions are not exported as they are internal implementation details
  )
where


import Yaifl.Prelude

import Yaifl.Actions.Args
import Yaifl.Property.Has ( WMWithProperty )
import Yaifl.Object.Query ( getThingMaybe )
import Yaifl.Effects.RuleEffects
import Yaifl.Text.SayableValue
import Yaifl.Thing.Kind
import qualified Data.List.NonEmpty as NE
import Yaifl.Container.Kind
import Yaifl.Text.Say
import Yaifl.Rulebook
import Yaifl.Enclosing.Query
import Yaifl.Object.Kind
import Yaifl.MultiLocated.Kind (MultiLocated)

-- | Main accessibility rulebook.
--
-- This rulebook determines whether the source object can access the target object.
-- It checks various constraints including spatial relationships and container access.
--
-- The rulebook takes 'Args' containing:
-- - source: The object trying to access something
-- - variables: The target object being accessed
--
-- Returns 'True' if access is possible, 'False' if any accessibility constraint fails.
-- Also generates appropriate error messages when access is blocked.
accessibility ::
  WithPrintingNameOfSomething wm
  => WMWithProperty wm MultiLocated
  => WMWithProperty wm Container
  => Rulebook wm Unconstrained (Args wm (Thing wm)) Bool
accessibility = Rulebook
    "accessibility"
    Nothing
    [ makeRule "in the same place" [] samePlace
    , notImplementedRule "can't reach inside rooms"
    , makeRule "can't reach inside closed containers" [] insideClosedContainers
    , notImplementedRule "can't reach outside closed containers"
    ]

-- | Check if source and target are in the same location.
--
-- This rule returns 'True' if any of the following conditions are met:
-- - Both objects are contained by the same parent object
-- - The source is contained by the target
-- - The target is contained by the source
--
-- This is the most basic accessibility check that allows interaction between
-- objects that are spatially co-located.
samePlace :: Args wm (Thing wm) -> Eff es (Maybe Bool)
samePlace Args{source=s, variables=v} =
  return $ if
    (thingContainedBy s `objectEquals` thingContainedBy v)
    || (thingContainedBy s `objectEquals` v)
    || (s `objectEquals` thingContainedBy v) then Just True else Nothing

-- | Check if access is blocked by closed containers.
--
-- This rule analyses the containment hierarchy between source and target to determine
-- if any closed containers would block access. It traverses the containment chain
-- from target back to the common ancestor, checking each container along the path.
--
-- If a closed container is found in the path, an appropriate error message is generated
-- and the rule returns 'False'. Otherwise, it returns 'Nothing' to allow other rules
-- to determine accessibility.
insideClosedContainers ::
  WithPrintingNameOfSomething wm
  => WMWithProperty wm MultiLocated
  => RuleEffects wm es
  => WMWithProperty wm Container
  => Args wm (Thing wm) -> Eff es (Maybe Bool)
insideClosedContainers Args{source=s, variables=v}= do
  commonAncestor <- getCommonAncestor s v
  -- this is everything (which should be nonempty) we may need to reach through
  manyHiers <- filter (not . null) . toList . NE.map (NE.takeWhile (/= commonAncestor)) <$> getContainingHierarchies v
  let accessibilityHierarchyError source target hierarchies =
        "Accessibility hierarchy error: expected one containment hierarchy but got " <>
        show (length hierarchies) <> " hierarchies while checking access from " <>
        display (source ^. #name) <> " to " <> display (target ^. #name) <> ": " <> show hierarchies
      hier = case manyHiers of
        [x] -> x
        x -> error $ accessibilityHierarchyError s v x
  anyClosed <- (join . listToMaybe . catMaybes) <$$> forM (reverse hier) $ \barrier -> do
    barrierThing <- getThingMaybe barrier
    let isCC = (\t -> isClosedContainer <$?> getContainerMaybe t) <$> barrierThing
    if isCC == Just True then return $ Just barrierThing else return Nothing
  case anyClosed of
    Just x -> do
      [saying|{The x} #{aren't} open.|]
      return $ Just False
    Nothing -> return Nothing
