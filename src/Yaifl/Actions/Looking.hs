module Yaifl.Actions.Looking
--( lookingActionImpl
--, HasLookingProperties
--) where
  where

import Yaifl.Prelude
import Yaifl.Common
import Yaifl.ObjectLookup
import Yaifl.Properties
import Yaifl.ObjectLogging
import qualified Data.EnumSet as DES
import qualified Prettyprinter.Render.Terminal as PPTTY
import Yaifl.Actions.Common
import Yaifl.Rulebooks
import Yaifl.Messages
{-
type HasLookingProperties s = (HasProperty s Enclosing, HasProperty s Enterable, HasProperty s Container)

data LookingActionVariables s = LookingActionVariables
  { _lookingFrom :: !(AnyObject s)
  , _visibilityCount :: !Int
  , _visibilityLevels :: [AnyObject s]
  , _roomDescribingAction :: !Text
  }
  deriving (Show)

instance Prettify (LookingActionVariables s) where
  prettify (LookingActionVariables fr _ lvls _) = "Looking from "
    <> shortPrint fr <> " with levels " <> mconcat (map shortPrint lvls)

lookingActionImpl
  :: HasLookingProperties s
  => Action s
lookingActionImpl = Action
  "looking"
  ["look", "looking"]
  (ParseArguments lookingActionSet)
  (makeActionRulebook "before looking rulebook" [])
  (makeActionRulebook "check looking rulebook" [])
  carryOutLookingRules
  (makeActionRulebook "report looking rulebook" [])

-- if we have no source, then we have no idea where we are looking 'from'; return nothing
-- lightLevels (recalc light) is how many levels we can actually see because of light
-- vl is how many levels we could see in perfect light.
-- so if there's no light at all, then we take none of the levels - even if we could potentially see
-- 100 up.
lookingActionSet
  :: forall s.
  HasLookingProperties s
  => UnverifiedArgs s
  -> World s
  -> Maybe (LookingActionVariables s)
lookingActionSet (UnverifiedArgs Args{..}) w = do
  (asThing :: Maybe (Thing s)) <- fromAny <$> _argsSource ^? _Just
  asThing' <- asThing
  loc <- asThing ^? _Just % containedBy
  reifyLoc <- getObject' loc w
  vl <- getVisibilityLevels reifyLoc w
  let lightLevels = (`recalculateLightOfParent` w) asThing'
  return $ LookingActionVariables reifyLoc lightLevels (take lightLevels vl) "looking"

getVisibilityLevels
  :: HasLookingProperties s
  => AnyObject s
  -> World s
  -> Maybe [AnyObject s]
getVisibilityLevels e w = do
  vh <- findVisibilityHolder e w
  if vh `eqObject` e
      then return [e]
      else (vh :) <$> getVisibilityLevels vh w

-- | the visibility holder of a room or an opaque, closed container is itself; otherwise, the enclosing entity
findVisibilityHolder
  :: HasLookingProperties s
  => AnyObject s
  -> World s
  -> Maybe (AnyObject s)
findVisibilityHolder e' w =
  if
    isRoom e' || isOpaqueClosedContainer e' w
  then
    Just e'
  else
    do
      t <- getThing' e' w ^? _Just % containedBy
      getObject' t w

-- Inform Designer's Manual, Page 146
-- we recalculate the light of the immediate holder of an object
-- there is light exactly when the parent (p) "offers light"
-- has light is if it's lit, or see through and it contains light
-- offers light means it lights INTO itself
-- has light means it lights OUT AWAY from itself
recalculateLightOfParent
  :: HasLookingProperties s
  => ObjectLike s o
  => o
  -> World s
  -> Int
recalculateLightOfParent e w = case parent of
  Nothing -> 0
  Just p ->
    if
      fromMaybe False $ offersLight p w
    then
      maybe 0 (\v -> 1 + recalculateLightOfParent v w) parent
    else
      0
  where
    parent = view containedBy <$> getThing' e w

-- | An object offers light if:
-- - it is a lit thing (lit thing or lighted room)
-- - it contains a thing that has light
-- - it is see-through and its parent offers light
-- this goes DOWN the object tree; the light goes to its contents
offersLight
  :: HasLookingProperties s
  => ObjectLike s o
  => o
  -> World s
  -> Maybe Bool
offersLight e w = do
  -- this will short circuit if we can't find e
  objIsLit <- objectItselfHasLight e w
  -- it is an object (else Nothing)
  -- it is see through
  -- its parent offers light
  let seeThruWithParent = maybe False
        (\o -> all (( == True) . fromMaybe False) [Just $ isSeeThrough o w, parentOffersLight o w]) (getThing' e w)
      parentOffersLight o = offersLight (o ^. containedBy)
  return $ objIsLit || seeThruWithParent || containsLitObj e w

-- | an object is see through if it's transparent, a supporter, an open container, or enterable but not a container
isSeeThrough
  :: HasLookingProperties s
  => Thing s
  -> World s
  -> Bool
isSeeThrough e w = isSupporter || isTransparent || isEnterableNotContainer || isOpenContainer where
  isSupporter = isType e (ObjType "supporter") w
  isContainer = isType e (ObjType "container") w
  c = getContainer' e w
  en = getEnterable' e w
  isTransparent = fmap _containerOpacity c == Just Transparent
  isOpenContainer = (fmap _containerOpenable c == Just Open) && isContainer
  isEnterableNotContainer = en == Just Enterable && not isContainer

containsLitObj
  :: HasLookingProperties s
  => ObjectLike s o
  => o
  -> World s
  -> Bool
containsLitObj e w = maybe False (\e' -> any hasLightOrNothing (DES.elems $ e' ^. enclosingContains)) (getEnclosing' e w)
  where
    hasLightOrNothing o = fromMaybe False (hasLight o w)

-- | either a lit object or a lighted room
objectItselfHasLight
  :: ObjectLike s o
  => o
  -> World s
  -> Maybe Bool
objectItselfHasLight e = asThingOrRoom' e
  (\x -> x ^. objData % thingLit == Lit)
  (\x -> x ^. objData % roomDarkness == Lighted)

-- | (4) An object has light if:
-- (a) it itself has the light attribute set, or
-- (b) it is see-through and any of its immediate possessions have light, or
-- (c) any object it places in scope using the property add_to_scope has light.
-- ignoring (c) for now; TODO?
-- this goes UP the object tree
hasLight
  :: HasLookingProperties s
  => ObjectLike s o
  => o
  -> World s
  -> Maybe Bool
hasLight e w = do
  -- short circuits if o is an invalid object
  litObj <- objectItselfHasLight e w
  let isSeeThroughObj = maybe False (`isSeeThrough` w) (getThing e w)
  return $ litObj || isSeeThroughObj && containsLitObj e w

carryOutLookingRules :: ActionRulebook s (LookingActionVariables s)
carryOutLookingRules = makeActionRulebook "Carry Out Looking" [
  makeRule "room description heading rule"
        (\rb -> do
          setSayStyle (Just PPTTY.bold)
          let (LookingActionVariables _ cnt lvls _) = _argsVariables rb
          --logVerbose $ prettify (_argsVariables rb)
          let visCeil = viaNonEmpty last lvls
          --logVerbose $
          --  "Printing room description heading with visibility ceiling " <> prettify (shortPrint <$> visCeil) <>
         --     " and visibility count " <> show cnt
        {-}
          if
            | cnt == 0 -> do
              doActivity' printingNameOfADarkRoom ()
              pass --no light, print darkness
            | visCeil == getID loc ->
              traverse_ printName visCeil --if the ceiling is the location, then print [the location]
            | True ->
              traverse_ (`printNameEx` capitalThe) visCeil --otherwise print [The visibility ceiling]
          mapM_ foreachVisibilityHolder (drop 1 lvls)
          modify $ sayLn ""
          modify $ setSayStyle Nothing
          --TODO: "run paragraph on with special look spacing"?
        -}
          return Nothing)

  ]
{-


carryOutLookingRules :: HasPhysicalStore w => LookingActionVariables -> Rulebook w LookingActionVariables RuleOutcome
carryOutLookingRules =
  makeRulebookWithVariables
    "carry out looking rulebook"
    [ RuleWithVariables

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
-}