module Yaifl.Std.Rulebooks.Accessibility
  ( accessibility

  ) where


import Yaifl.Prelude

import Yaifl.Core.Actions.Args
import Yaifl.Core.WorldModel ( WMWithProperty )
import Yaifl.Core.Query.Object ( getThingMaybe )
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Text.SayableValue
import Yaifl.Core.Kinds.Thing
import qualified Data.List.NonEmpty as NE
import Yaifl.Std.Kinds.Container
import Yaifl.Text.Say
import Yaifl.Core.Rules.Rulebook
import Yaifl.Core.Query.Enclosing
import Yaifl.Core.Kinds.Object

accessibility ::
  WithPrintingNameOfSomething wm
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

samePlace :: Args wm (Thing wm) -> Eff es (Maybe Bool)
samePlace Args{source=s, variables=v} =
  return $ if any id
    [ (thingContainedBy s) `objectEquals` (thingContainedBy v)
    , (thingContainedBy s) `objectEquals` v
    , s `objectEquals` (thingContainedBy v)
    ] then Just True else Nothing

insideClosedContainers ::
  WithPrintingNameOfSomething wm
  => RuleEffects wm es
  => WMWithProperty wm Container
  => Args wm (Thing wm) -> Eff es (Maybe Bool)
insideClosedContainers Args{source=s, variables=v}= do
  commonAncestor <- getCommonAncestor s v
  -- this is everything (which should be nonempty) we may ned to reach through
  hier <- NE.takeWhile (/= commonAncestor) <$> getContainingHierarchy v
  anyClosed <- (join . listToMaybe . catMaybes) <$$> forM (reverse hier) $ \barrier -> do
    barrierThing <- getThingMaybe barrier
    let isCC = (\t -> isClosedContainer <$?> getContainerMaybe t) <$> barrierThing
    if isCC == Just True then return $ Just barrierThing else return Nothing
  case anyClosed of
    Just x -> do
      [saying|{The x} #{aren't} open.|]
      return $ Just False
    Nothing -> return Nothing
