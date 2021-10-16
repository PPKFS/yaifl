module Yaifl.Actions
  ( addBaseActions
  )
where

import Yaifl.Common
import Yaifl.Prelude
import Yaifl.Objects


addAction
  :: Action s
  -> World s
  -> World s
addAction ac =
  actions % at (_actionName ac) ?~ ac

addBaseActions
  :: HasProperty s Container
  => World s
  -> World s
addBaseActions = foldr (.) id [
    addAction lookingActionImpl
  ]

makeActionRulebook
  :: Text
  -> [Rule o (Args o v) RuleOutcome]
  -> ActionRulebook o v
makeActionRulebook n = Rulebook n Nothing (const . Just)

data LookingActionVariables = LookingActionVariables
  { _visibilityCount :: Int,
    _visibilityLevels :: [Entity],
    _roomDescribingAction :: Text
  }
  deriving (Show)

lookingActionImpl
  :: HasProperty s Container
  => Action s
lookingActionImpl = Action
  "looking"
  ["look", "looking"]
  lookingActionSet
  (makeActionRulebook "before looking rulebook" [])
  (makeActionRulebook "check looking rulebook" [])
  carryOutLookingRules
  (makeActionRulebook "report looking rulebook" [])

carryOutLookingRules :: ActionRulebook s LookingActionVariables
carryOutLookingRules = makeActionRulebook "Carry Out Looking" []

-- if we have no source, then we have no idea where we are looking 'from'; return nothing
-- lightLevels (recalc light) is how many levels we can actually see because of light
-- vl is how many levels we could see in perfect light.
-- so if there's no light at all, then we take none of the levels - even if we could potentially see
-- 100 up.
lookingActionSet
  :: forall s.
  HasProperty s Container
  => UnverifiedArgs s
  -> World s
  -> Maybe LookingActionVariables
lookingActionSet Args{..} w = do
  (asThing :: Maybe (Thing s)) <- fromAny <$> _argsSource ^? _Just
  asThing' <- asThing
  loc <- asThing ^? _Just % containedBy
  vl <- getVisibilityLevels loc w
  lightLevels <- (`recalculateLightOfParent` w) asThing'
  return $ LookingActionVariables lightLevels (take lightLevels vl) "looking"

getVisibilityLevels
  :: HasProperty s Container
  => ObjectLike s o
  => o
  -> World s
  -> Maybe [Entity]
getVisibilityLevels e w = case findVisibilityHolder e w of
    Nothing -> Nothing
    Just a' -> if a' == e'
        then Just [e']
        else (a' :) <$> getVisibilityLevels a' w
    where e' = getID e

-- | the visibility holder of a room or an opaque, closed container is itself; otherwise, the enclosing entity
findVisibilityHolder
  :: HasProperty s Container
  => ObjectLike s o
  => o
  -> World s
  -> Maybe Entity
findVisibilityHolder e' w =
  if
    isRoom e' || isOpaqueClosedContainer e' w
  then
    Just (getID e')
  else
    getThing' e' w ^? _Just % containedBy

-- Inform Designer's Manual, Page 146
-- we recalculate the light of the immediate holder of an object
-- there is light exactly when the parent (p) "offers light"

-- has light is if it's lit, or see through and it contains light
-- offers light means it lights INTO itself
-- has light means it lights OUT AWAY from itself
recalculateLightOfParent
  :: ObjectLike s o
  => o
  -> World s
  -> Maybe Int
recalculateLightOfParent e w = do
  -- TODO: verify if this is fine failing on a room
  loc <- getThing' e w
  offers <- offersLight loc w
  if offers then (do
    recurs <- recalculateLightOfParent loc w
    return (1 + recurs)
    )
  else
    return 0

-- | A thing offers [out] light if:
-- - it is a lit thing (lit thing or lighted room)
-- - it contains a thing that has light
-- - it is see-through and its parent offers light
offersLight
  :: ObjectLike s o
  => o
  -> World s
  -> Maybe Bool
offersLight e w = orM [objIsLit, containsLitObj, seeThruWithParent] where
  objIsLit = objectItselfHasLight e w
  seeThruWithParent = (\o -> all (== Just True) [Just $ isSeeThrough o w, parentOffersLight o w]) <$> getThing' e w
  parentOffersLight o = offersLight (o ^. containedBy)
  containsLitObj = Nothing --maybeM False (anyM hasLight) $ l3 ^? _Just . encloses

-- | an object is see through if it's transparent, a supporter, an open container, or enterable but not a container
isSeeThrough
  :: Thing s
  -> World s
  -> Bool
isSeeThrough e w = isSupporter || isTransparent || isEnterableNotContainer || isOpenContainer where
  isSupporter = isType e "supporter" w
  isContainer = isType e "container" w
  c = getContainer' e w
  en = getEnterable' e w
  isTransparent = fmap _containerOpacity c == Just Transparent
  isOpenContainer = (fmap _containerOpenable c == Just Open) && isContainer
  isEnterableNotContainer = en == Just Enterable && not isContainer

-- either a lit object or a lighted room
objectItselfHasLight
  :: ObjectLike s o
  => o
  -> World s
  -> Maybe Bool
objectItselfHasLight e = asThingOrRoom' e
  (\x -> x ^. objData % thingLit == Lit)
  (\x -> x ^. objData % roomDarkness == Lighted)
{-
hasLight :: WithStandardWorld w m => Entity -> m Bool
hasLight e = do
  litObj <- objectItselfHasLight e
  l1 <- getComponent @Enclosing e
  l2 <- isSeeThrough e
  containsLitObj <- maybeM False (anyM hasLight) $ l1 ^? _Just . encloses
  return $ litObj || l2 && containsLitObj







carryOutLookingRules :: HasPhysicalStore w => LookingActionVariables -> Rulebook w LookingActionVariables RuleOutcome
carryOutLookingRules =
  makeRulebookWithVariables
    "carry out looking rulebook"
    [ RuleWithVariables
        "room description heading rule"
        do
          setStyle (Just PPTTY.bold)
          LookingActionVariables cnt lvls _ <- getRulebookVariables
          let visCeil = viaNonEmpty last lvls
          loc <- getActor >>= getLocation
          logDebug $
            "Printing room description heading with visibility ceiling ID " <> show visCeil
              <> " and visibility count "
              <> show cnt

          if
              | cnt == 0 -> do
                doActivity printingNameOfADarkRoomName []
                pass --no light, print darkness
              | visCeil == loc ->
                traverse_ printName visCeil --if the ceiling is the location, then print [the location]
              | True ->
                traverse_ (`printNameEx` capitalThe) visCeil --otherwise print [The visibility ceiling]
          mapM_ foreachVisibilityHolder (drop 1 lvls)
          lift $ sayLn ""
          lift $ setStyle Nothing
          --TODO: "run paragraph on with special look spacing"?
          return Nothing,
      RuleWithVariables
        "room description body rule"
        do
          LookingActionVariables cnt lvls ac <- getRulebookVariables
          let visCeil = viaNonEmpty last lvls
          loc <- getActor >>= getLocation
          roomDesc <- use roomDescriptions
          dw <- use darknessWitnessed
          let abbrev = roomDesc == AbbreviatedRoomDescriptions
              someAbbrev = roomDesc == SometimesAbbreviatedRoomDescriptions
          if
              | cnt == 0 ->
                unless
                  (abbrev || (someAbbrev && dw))
                  do
                    _ <- doActivity' printingDescriptionOfADarkRoomName
                    pass
              | visCeil == loc ->
                unless
                  (abbrev || (someAbbrev && ac /= lookingActionName))
                  ( do
                      desc <- traverse evalDescription loc
                      whenJust desc sayLn
                  )
              | otherwise -> pass
          return Nothing,
      RuleWithVariables
        "room description paragraphs about objects rule"
        do
          LookingActionVariables _ lvls _ <- getRulebookVariables
          mapM_ (doActivity describingLocaleActivityName . return) lvls
          return Nothing
    ]

foreachVisibilityHolder :: (WithGameData w m, HasObjectStore w) => Entity -> RuleVarsT LookingActionVariables m ()
foreachVisibilityHolder e = do
  ifM (e `isType` "supporter") (say "(on ") (say "(in ")
  printName e
  say ")"
-}