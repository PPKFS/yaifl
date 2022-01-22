module Yaifl.Activities.ChoosingNotableLocaleObjects
( choosingNotableLocaleObjectsImpl
) where

import Yaifl.Common
import Yaifl.Activities.Common
import Yaifl.Prelude
import Yaifl.Rulebooks
import Yaifl.Properties
import Yaifl.ObjectLookup
import qualified Data.EnumMap as DEM
import qualified Data.EnumSet as DES
import Yaifl.ObjectLogging (objectName)

choosingNotableLocaleObjectsImpl
  :: HasProperty s Enclosing
  => Activity s (AnyObject s) (LocalePriorities s)
choosingNotableLocaleObjectsImpl = makeActivity "Choosing notable locale objects" $ makeRule "" 
  (\v -> do
    n <- objectName v
    e' <- getEnclosing v 
    case e' of
      Nothing -> (do
        warn $ bformat ("Tried to choose notable locale objects from " 
          %! stext %! " which that doesn't enclose ") n
        return Nothing)
      Just encl -> (do
        l <- mapM (\x -> do
          debug $ bformat ("Found a " %! stext) n
          getObject x) (DES.toList (_enclosingContains encl))
        return (Just (Store $ DEM.fromList $ map (\x -> (getID x, LocaleInfo 5 x False)) l)))
  )

{-
removeFromLocale
  :: Entity
  -> LocaleVariables s
  -> LocaleVariables s
removeFromLocale o = localePriorities % at o .~ Nothing

type HasLocaleProperties s = (HasProperty s Enclosing, HasProperty s Container, HasProperty s Openable, HasProperty s ThingLit)
--something about having omit contents from listing here
--are identically named TODO??? about matching containers???
groupingEquivalenceRelation
  :: HasLocaleProperties s
  => ObjectLike s o1
  => ObjectLike s o2
  => World s
  -> o1
  -> o2
  -> Bool
groupingEquivalenceRelation w e1 e2 = not (
    (hasChildren e1 w && willRecurse e1 w)
    || (hasChildren e2 w && willRecurse e2 w)
    || (getWornBy' e1 w /= getWornBy' e2 w)
    || (getThingLit' e1 w /= getThingLit' e2 w)
    || isMatchingContainers e1 e2 w)
    && (getName' e1 w == getName' e2 w)

--p2275 of the complete program, in B/lwt
isMatchingContainers
  :: HasProperty s Openable
  => ObjectLike s o1
  => ObjectLike s o2
  => o1
  -> o2
  -> World s
  -> Bool
isMatchingContainers e1 e2 = evalState $ do
  o <- getOpenable e1
  o2 <- getOpenable e2
  o1c <- gets $ e1 `isType` ObjType "container"
  o2c <- gets $ e2 `isType` ObjType "container"
  return $ (o1c == o2c) && o1c && (o == o2)

hasChildren
  :: HasProperty s Enclosing
  => ObjectLike s o
  => o
  -> World s
  -> Bool
hasChildren e = maybe False (\e' -> DES.size (_enclosingContains e') == 0) <$> getEnclosing' e

willRecurse
  :: HasProperty s Container
  => ObjectLike s o
  => o
  -> World s
  -> Bool
willRecurse e = evalState $ do
  isSup <- gets $ e `isType` ObjType "supporter"
  cont <- getContainer e
  return $ isSup || maybe False (\c -> _containerOpacity c == Transparent || _containerOpenable c == Open) cont


isLocaleSupported :: forall w m. WithStandardWorld w m => Entity -> m Bool
isLocaleSupported it = do
  isScenery <- it `isType` "scenery"
  ism <- isMentioned it
  isd <- isDescribed it
  return $ not $ isScenery || ism || isd

isMentioned :: WithGameData w m => Entity -> m Bool
isMentioned e = uses (localeData . mentionedThings) (e `DS.member`)

data ListingContentsArgs = ListingContentsArgs
  { _entity :: Entity,
    _isMarkedOnly :: Bool,
    _isIgnoringConcealed :: Bool
  }
  deriving (Eq, Show)

listingContentsArgs :: (Int -> Bool, [Entity] -> Maybe ListingContentsArgs)
listingContentsArgs =
  ( \x -> x >= (1 :: Int) && x < 4,
    \case
      [x] -> Just $ ListingContentsArgs x False False
      x : xs -> Just $ ListingContentsArgs x (markedOnlyFlag `elem` xs) (noConcealedFlag `elem` xs)
      _ -> Nothing
  )

listingContentsOfSomethingImpl :: forall w. HasStandardWorld w => Activity w ListingContentsArgs
listingContentsOfSomethingImpl =
  makeActivity
    listingContentsOfSomethingName
    listingContentsArgs
    ( do
        ListingContentsArgs e markedOnly ignoreConcealedItems <- getRulebookVariables
        l <- (toList . _encloses) <<$>> getComponent @Enclosing e
        contentsToList <-
          maybe
            (return [])
            ( filterM
                ( \x -> do
                    ml <- isX @(Physical w) True _markedForListing x
                    c <- isConcealed x
                    return $ (not markedOnly || ml) && (not ignoreConcealedItems || c)
                )
            )
            l
        mapM_
          ( \(a, v) -> do
              printNameEx a (SayOptions Indefinite Uncapitalised)
              when (v < length contentsToList - 1) (say ", ")
              when (v == length contentsToList - 2) (say "and ")
              mentionThing a
          )
          $ zip (toList contentsToList) [0 ..]
        return $ Just True
    )
  -}