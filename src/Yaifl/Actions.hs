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
  loc <- asThing ^? _Just % containedBy
  let vl = getVisibilityLevels loc w
  lightLevels <- (`recalculateLightOfParent` w) <$> asThing
  return $ LookingActionVariables lightLevels (take lightLevels vl) "looking" 

getVisibilityLevels
  :: HasProperty s Container
  => ObjectLike o
  => o
  -> World s
  -> [Entity]
getVisibilityLevels e w = case findVisibilityHolder e w of
    Nothing -> []
    Just a' -> if a' == e'
        then [e']
        else a' : getVisibilityLevels a' w
    where e' = getID e

-- | the visibility holder of a room or an opaque, closed container is itself; otherwise, the enclosing entity
findVisibilityHolder 
  :: HasProperty s Container
  => ObjectLike o
  => o
  -> World s
  -> Maybe Entity
findVisibilityHolder e' w = if isRoom e' || isOpaqueClosedContainer e' w then Just (getID e') else evalState (do
  t <- getThing e'
  return $ t ^? _Just % containedBy) w
  

-- Inform Designer's Manual, Page 146
-- we recalculate the light of the immediate holder of an object
-- there is light exactly when the parent (p) "offers light"

-- has light is if it's lit, or see through and it contains light
-- offers light means it lights INTO itself
-- has light means it lights OUT AWAY from itself
recalculateLightOfParent 
  :: ObjectLike o
  => o
  -> World s
  -> Int
recalculateLightOfParent e w = maybe 0 ( \p' -> 
  if 
    offersLight w p' 
  then
    1 + recalculateLightOfParent p' w
  else
    0) (getLocation' e w)
  

offersLight = undefined
getLocation' 
  :: ObjectLike o
  => o
  -> World w
  -> Maybe Entity
getLocation' = undefined
{-}
-- offering light is a lit thing (lit thing or lighted room), or has a thing that has light, or is see-through and its parent offers light
offersLight :: forall w m. WithStandardWorld w m => Entity -> m Bool
offersLight e = do
  litObj <- objectItselfHasLight e
  l3 <- getComponent @Enclosing e
  l4 <- isSeeThrough e >>= (\x -> if x then getLocation e >>= maybeM False offersLight else return False)
  containsLitObj <- maybeM False (anyM hasLight) $ l3 ^? _Just . encloses
  return $ litObj || l4 || containsLitObj

-- either a lit object or a lighted room
objectItselfHasLight :: forall w m. WithStandardWorld w m => Entity -> m Bool
objectItselfHasLight e = do
  l1 <- getComponent @(Thing w) e
  l2 <- getComponent @RoomData e
  return $ (l1 ^? _Just . thingPhysical . lit == Just Lit) || (l2 ^? _Just . darkness == Just Lighted)

hasLight :: WithStandardWorld w m => Entity -> m Bool
hasLight e = do
  litObj <- objectItselfHasLight e
  l1 <- getComponent @Enclosing e
  l2 <- isSeeThrough e
  containsLitObj <- maybeM False (anyM hasLight) $ l1 ^? _Just . encloses
  return $ litObj || l2 && containsLitObj

--an object is see through if it's transparent, a supporter, an open container, or enterable but not a container
isSeeThrough :: WithStandardWorld w m => Entity -> m Bool
isSeeThrough e = do
  l1 <- getComponent @ContainerData e
  l2 <- getComponent @Openable e
  l3 <- getComponent @Supporter e
  l4 <- getComponent @Enterable e
  isContainer <- e `isType` "container"
  let isTransparent = fmap _opacity l1 == Just Transparent
      isOpenContainer = l2 == Just Open && isJust l1
      isEnterableNotContainer = l4 == Just Enterable && not isContainer
  return $ isJust l3 || isTransparent || isEnterableNotContainer || isOpenContainer





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