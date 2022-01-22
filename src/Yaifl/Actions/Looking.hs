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
import qualified Data.Text.Lazy.Builder as TLB
import Yaifl.Activities.Common
import Yaifl.Activities.PrintingNameOfSomething (printName, capitalThe, printNameEx)

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

lookingActionName :: Text 
lookingActionName = "looking"
lookingActionImpl ::
  HasLookingProperties s
  => Action s
lookingActionImpl = Action
  lookingActionName
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
lookingActionSet ::
  forall s m. HasLookingProperties s
  => MonadWorld s m
  => UnverifiedArgs s
  -> m (Maybe (LookingActionVariables s))
lookingActionSet (UnverifiedArgs Args{..}) = withoutMissingObjects (runMaybeT $ do
  as <- hoistMaybe $ _argsSource ^? _Just
  (t :: Thing s) <- hoistMaybe $ fromAny as
  reifyLoc <- getObject (t ^. containedBy)
  vl <- getVisibilityLevels reifyLoc
  lightLevels <- recalculateLightOfParent t
  return $ LookingActionVariables reifyLoc lightLevels (take lightLevels vl) "looking") (handleMissingObject "" Nothing)

getVisibilityLevels
  :: MonadWorld s m
  => NoMissingObjects s m
  => HasLookingProperties s
  => AnyObject s -- ^ 
  -> m [AnyObject s]
getVisibilityLevels e = do
  vh <- findVisibilityHolder e
  if vh `eqObject` e
      then return [e]
      else (vh :) <$> getVisibilityLevels vh

-- | the visibility holder of a room or an opaque, closed container is itself; otherwise, the enclosing entity
findVisibilityHolder ::
  MonadWorld s m
  => HasLookingProperties s
  => NoMissingObjects s m
  => ObjectLike s o
  => CanBeAny o (AnyObject s)
  => o
  -> m (AnyObject s)
findVisibilityHolder e' = do
  mCont <- getContainer e'
  n <- objectName e'
  if
    isRoom e' || isOpaqueClosedContainer <$?> mCont
  then
    do
      debug $ bformat ("The visibility holder of " %! stext %! " is itself ") n
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
  s <- isSupporter e
  isContainer <- isType e (ObjType "container")
  let isOpenContainer = fmap _containerOpenable c == Just Open && isContainer
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
  => o -- ^ the object
  -> m Bool
containsLitObj e = do
  enc <- getEnclosing e
  case enc of
    Nothing -> return False
    Just encs -> anyM hasLight (DES.elems $ encs ^. enclosingContains)


{- | (4) An object itself has light if:  
  (a) it's a room with the lighted property,  
  (b) it's a thing with the lit property.  
  If you want to include transitive light, you want `hasLight`.
-}
objectItselfHasLight
  :: NoMissingObjects s m
  => ObjectLike s o
  => MonadWorld s m
  => o -- ^ the object
  -> m Bool
objectItselfHasLight e = asThingOrRoom' e
  (\x -> x ^. objData % thingLit == Lit)
  (\x -> x ^. objData % roomDarkness == Lighted)

{- | (4) An object has light if:  
  (a) it itself has the light attribute set, or  
  (b) it is see-through and any of its immediate possessions have light, or  
  (c) any object it places in scope using the property add_to_scope has light.  
  ignoring (c) for now; TODO?  
  this goes UP the object tree; it provides light TO its surroundings.  
-}
hasLight
  :: NoMissingObjects s m
  => HasLookingProperties s
  => MonadWorld s m
  => ObjectLike s o
  => o
  -> m Bool
hasLight e = do
  ts <- getThingMaybe e
  objectItselfHasLight e
    ||^ (maybe (return False) isSeeThrough ts
      &&^ containsLitObj e)

carryOutLookingRules :: ActionRulebook s (LookingActionVariables s)
carryOutLookingRules = makeActionRulebook "Carry Out Looking" [
  makeRule "room description heading rule"
    (\rb -> do
      setSayStyle (Just PPTTY.bold)
      let (LookingActionVariables loc cnt lvls _) = _argsVariables rb
          visCeil = viaNonEmpty last lvls
      debug $ TLB.fromText $ prettify (_argsVariables rb)
      debug (bformat
        ("Printing room description heading with visibility ceiling " %! stext %! " and visibility count " %! int)
        (prettify (shortPrint <$> visCeil))
        cnt)
      if
        | cnt == 0 -> do
          doActivity printingNameOfADarkRoom ()
          pass --no light, print darkness
        | (getID <$> visCeil) == Just (getID loc) ->
          traverse_ printName visCeil --if the ceiling is the location, then print [the location]
        | True ->
          traverse_ (`printNameEx` capitalThe) visCeil --otherwise print [The visibility ceiling]
      mapM_ foreachVisibilityHolder (drop 1 lvls)
      sayLn ""
      setSayStyle Nothing
      --TODO: "run paragraph on with special look spacing"?
      return Nothing),
  makeRule "room description body rule"
    (\rb -> do
      let (LookingActionVariables loc cnt lvls ac) = _argsVariables rb
          visCeil = viaNonEmpty last lvls
      roomDesc <- use roomDescriptions
      dw <- use darknessWitnessed
      let abbrev = roomDesc == AbbreviatedRoomDescriptions
          someAbbrev = roomDesc == SometimesAbbreviatedRoomDescriptions
      if
        | cnt == 0 -> 
          unless (abbrev || (someAbbrev && dw)) $ do
            doActivity printingDescriptionOfADarkRoom () 
            pass
        | (getID <$> visCeil) == Just (getID loc) ->
          unless (abbrev || (someAbbrev && ac /= lookingActionName)) $ do
            let desc = _objDescription loc
            unless (isBlankDescription desc)
              (sayLn (_objDescription loc))
        | otherwise -> pass
      return Nothing),
  makeRule "room description paragraphs about objects rule"
    (\rb -> do
      let (LookingActionVariables _ _ lvls _) = _argsVariables rb

      mapM_ (\o -> doActivity describingLocale (LocaleVariables blank o 0)) lvls
      return Nothing)
  ]

foreachVisibilityHolder :: 
  NoMissingObjects s m
  => MonadWorld s m
  => AnyObject s
  -> m ()
foreachVisibilityHolder e = do
  ifM (isSupporter e) (say "(on ") (say "(in ")
  printName e
  say ")"