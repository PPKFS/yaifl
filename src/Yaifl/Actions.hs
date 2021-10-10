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
  :: World s
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
  :: Action s
lookingActionImpl = Action
  "looking"
  ["look", "looking"]
  lookingActionSet
  (makeActionRulebook "before looking rulebook" [])
  (makeActionRulebook "check looking rulebook" [])
  carryOutLookingRules
  (makeActionRulebook "report looking rulebook" [])

carryOutLookingRules :: ActionRulebook s LookingActionVariables
carryOutLookingRules = makeActionRulebook "a" []

lookingActionSet
  :: UnverifiedArgs s
  -> World s
  -> Maybe LookingActionVariables
lookingActionSet args w = fmap (\x -> LookingActionVariables lightLevels (take lightLevels x) "looking") vl
            where
              o = _argsSource args
              loc = fromAny o ^? _Just % containedBy
              vl = (`getVisibilityLevels` w) =<< loc
              lightLevels = recalculateLightOfParent o

getVisibilityLevels
  :: ObjectLike o
  => o
  -> World s
  -> Maybe [Entity]
getVisibilityLevels e = evalState $ do
  a <- findVisibilityHolder e
  case a of
    Nothing -> return Nothing
    Just a' ->
      if a' == e
        then return (Just [getID e])
        else do
          logDebug $ "Visibility holder of " <> show e <> " was " <> show a'
          (a' :) <<$>> getVisibilityLevels a'

-- Inform Designer's Manual, Page 146
-- we recalculate the light of the immediate holder of an object
-- there is light exactly when the parent (p) "offers light"

---has light is if it's lit, or see through and it contains light
-- offers light means it lights INTO itself
-- has light means it lights OUT AWAY from itself
recalculateLightOfParent 
  :: ObjectLike o
  => o
  -> Entity 
  -> World s
  -> Int
recalculateLightOfParent e = do
  p <- getLocation e
  maybe
    (return 0)
    ( \p' ->
        ifM
          (offersLight p')
          ((1 +) <$> recalculateLightOfParent p')
          (return 0)
    )
    p
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



findVisibilityHolder :: forall w m. (HasContainer w, HasPhysicalStore w, WithGameData w m) => Entity -> m (Maybe Entity)
findVisibilityHolder e' = do
  -- the visibility holder of a room or an opaque, closed container is itself; otherwise, the enclosing entity
  ifM (e' `isType` "room" <|=> isOpaqueClosedContainer e') (return $ Just e') (Just . _enclosedBy <$> getPhysical' e')

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