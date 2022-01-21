module Yaifl.Actions.Looking
  ( lookingActionImpl
  , HasLookingProperties
  ) where

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
  (makeActionRulebook "check looking rulebook" []) --carryOutLookingRules
  (makeActionRulebook "report looking rulebook" [])

-- if we have no source, then we have no idea where we are looking 'from'; return nothing
-- lightLevels (recalc light) is how many levels we can actually see because of light
-- vl is how many levels we could see in perfect light.
-- so if there's no light at all, then we take none of the levels - even if we could potentially see
-- 100 up.
lookingActionSet
  :: forall s m. HasLookingProperties s
  => MonadWorld s m
  => UnverifiedArgs s
  -> m (Maybe (LookingActionVariables s))
lookingActionSet (UnverifiedArgs Args{..}) = withoutMissingObjects (runMaybeT $ do
  as <- hoistMaybe $ _argsSource ^? _Just
  (t :: Thing s) <- hoistMaybe $ fromAny as
  let loc = t ^. containedBy
  reifyLoc <- getObject loc
  let rl' = reifyLoc
  vl <- getVisibilityLevels rl'
  let lightLevels = 0 -- <- recalculateLightOfParent t
  return $ LookingActionVariables rl' lightLevels (take lightLevels vl) "looking") (handleMissingObject "" Nothing)

getVisibilityLevels
  :: MonadWorld s m
  => NoMissingObjects s m
  => HasLookingProperties s
  => AnyObject s
  -> m [AnyObject s]
getVisibilityLevels e = do
  vh <- findVisibilityHolder e
  if vh `eqObject` e
      then return [e]
      else (vh :) <$> getVisibilityLevels vh

-- | the visibility holder of a room or an opaque, closed container is itself; otherwise, the enclosing entity
findVisibilityHolder
  :: MonadWorld s m
  => HasLookingProperties s
  => NoMissingObjects s m
  => ObjectLike s o
  => CanBeAny o (AnyObject s)
  => o
  -> m (AnyObject s)
findVisibilityHolder e' = do
  mCont <- getContainer e'
  if
    isRoom e' || isOpaqueClosedContainer <$?> mCont
  then
    --return it
    return (toAny e')
  else
    do
      --get its container; we know it's a thing at this stage
      t <- getThing e'
      getObject (t ^. containedBy)

-- Inform Designer's Manual, Page 146
-- we recalculate the light of the immediate holder of an object
-- there is light exactly when the parent (p) "offers light"
-- has light is if it's lit, or see through and it contains light
-- offers light means it lights INTO itself
-- has light means it lights OUT AWAY from itself
recalculateLightOfParent
  :: NoMissingObjects s m
  => MonadWorld s m
  => HasLookingProperties s
  => ObjectLike s o
  => o
  -> m Int
recalculateLightOfParent e = do
  (parent :: Maybe Entity) <- view containedBy <$$> getThingMaybe e
  case parent of
    Nothing -> return 0
    Just p -> do
      ol <- offersLight p
      if 
        ol
      then 
        (1+) <$> recalculateLightOfParent p
      else
        return 0
    

-- | An object offers light if:
-- - it is a lit thing (lit thing or lighted room)
-- - it contains a thing that has light
-- - it is see-through and its parent offers light
-- this goes DOWN the object tree; the light goes to its contents
offersLight
  :: NoMissingObjects s m
  => MonadWorld s m
  => HasLookingProperties s
  => ObjectLike s o
  => o
  -> m Bool
offersLight e = do
  let parentOffersLight o = offersLight (o ^. containedBy)
      seeThruWithParent = maybe (return False) (\o' -> isSeeThrough o' &&^ parentOffersLight o')
  o <- getThingMaybe e

  objectItselfHasLight e -- it is a lit thing (lit thing or lighted room)
    ||^ seeThruWithParent o -- - it is see-through and its parent offers light
    ||^ containsLitObj e -- - it contains a thing that has light

-- | an object is see through if...
isSeeThrough
  :: NoMissingObjects s m
  => MonadWorld s m
  => HasLookingProperties s
  => Thing s
  -> m Bool
isSeeThrough e = do
  c <- getContainer e
  en <- getEnterable e
  s <- isType e (ObjType "supporter")
  isContainer <- isType e (ObjType "container")
  let isOpenContainer = (fmap _containerOpenable c == Just Open) && isContainer
      isTransparent = fmap _containerOpacity c == Just Transparent
      isEnterableNotContainer = en == Just Enterable && not isContainer
  return $ s --if it's a supporter
      || isTransparent -- it's transparent=
      || isEnterableNotContainer -- it's enterable but not a container
      || isOpenContainer -- it's an open container

containsLitObj
  :: NoMissingObjects s m
  => HasLookingProperties s
  => ObjectLike s o
  => MonadWorld s m
  => o
  -> m Bool
containsLitObj e = do
  enc <- getEnclosing e
  case enc of
    Nothing -> return False 
    Just encs -> anyM hasLight (DES.elems $ encs ^. enclosingContains)
    

-- | either a lit object or a lighted room
objectItselfHasLight
  :: NoMissingObjects s m
  => ObjectLike s o
  => MonadWorld s m
  => o
  -> m Bool
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
  :: NoMissingObjects s m
  => HasLookingProperties s
  => MonadWorld s m
  => ObjectLike s o
  => o
  -> m Bool
hasLight e = do
  ts <- getThingMaybe e
  let isSeeThroughObj = maybe (return False) isSeeThrough ts
      litObj = objectItselfHasLight e
  litObj ||^ (isSeeThroughObj &&^ containsLitObj e)

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