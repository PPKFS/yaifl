module Yaifl.Preconditions
  ( forPlayer
  , forPlayer'
  , forKind
  , theObject
  , theObject'
  , whenIn
  , whenPlayerIsIn
  , not_
  , whenPlayerIsInRegion
  , intoThe
  , whenContainsSomething
  ) where
import Yaifl.Prelude
import Yaifl.Rulebook
import Yaifl.Actions.Args
import Yaifl.Object.Kind
import Yaifl.Thing.Kind (Thing)
import Yaifl.Person.Query
import Yaifl.Metadata
import Yaifl.AnyObject
import Yaifl.ObjectLike
import Yaifl.TH
import Yaifl.MultiLocated.Kind
import Yaifl.Enclosing.Query
import Yaifl.Region.Kind
import Yaifl.Region.Query
import Yaifl.Backdrop.Kind
import Yaifl.Container.Kind
import Yaifl.Container.Query
import qualified Data.EnumSet as ES

forPlayer :: Precondition wm (Args wm v)
forPlayer = Precondition (pure "actor is the player") $ \v -> do
  p <- getPlayer
  pure $ p `objectEquals` (v ^. #source)

forPlayer' :: [Precondition wm (Args wm v)]
forPlayer' = [forPlayer]

forKind :: ObjectKind -> Precondition wm (Args wm (Thing wm))
forKind k = Precondition (pure $ "of kind " <> show k) $ \v -> variables v `isKind` k

theObject ::
  ArgsMightHaveMainObject v (Thing wm)
  => ThingLike wm o
  => o
  -> Precondition wm (Args wm v)
theObject o = Precondition
  { preconditionName = do
      e <- getThing o
      pure $ "to the object " <> display (e ^. #name)
  , checkPrecondition = \args -> do
      o' <- getThing o
      pure $ args ^? #variables % argsMainObjectMaybe == Just o'
  }

theObject' ::
  ThingLike wm o
  => o
  -> Precondition wm (AnyObject wm)
theObject' o = Precondition
  { preconditionName = do
      e <- getThing o
      pure $ "to the object " <> display (e ^. #name)
  , checkPrecondition = \args -> do
      o' <- getThing o
      pure $ args `objectEquals` o'
  }

whenIn ::
  ObjectLike wm e
  => WMWithProperty wm Backdrop
  => WMWithProperty wm MultiLocated
  => IsEnclosing e
  => e
  -> Precondition wm (Args wm v)
whenIn e = Precondition
  { preconditionName = do
      e' <- getObject e
      pure $ "when in the location " <> display (e' ^. #name)
  , checkPrecondition = \args -> do
      hierarchy <- getContainingHierarchies (args ^. #source)
      pure $ any (elem (getEnclosingEntity e)) hierarchy
  }

not_ ::
  Precondition wm a
  -> Precondition wm a
not_ prec = prec { checkPrecondition = fmap not . checkPrecondition prec}

whenPlayerIsIn ::
  ObjectLike wm e
  => WMWithProperty wm Backdrop
  => WMWithProperty wm MultiLocated
  => IsEnclosing e
  => e
  -> Precondition wm a
whenPlayerIsIn e = Precondition
  { preconditionName = do
      e' <- getObject e
      pure $ "when player is in the location " <> display (e' ^. #name)
  , checkPrecondition = const $ do
      hierarchy <- getPlayer' >>= getContainingHierarchies
      pure $ any (elem (getEnclosingEntity e)) hierarchy
  }

whenPlayerIsInRegion ::
  RegionEntity
  -> Precondition wm a
whenPlayerIsInRegion e = Precondition
    { preconditionName = do
      e' <- getRegion e
      pure $ "when player is in the region " <> display (e' ^. #name)
  , checkPrecondition = const $ do
      playerRoom <- getPlayerLocation
      regHierarchy <- getEnclosingRegions playerRoom
      pure $ e `elem` map tagRegionEntity regHierarchy
  }

aKindOf ::
  ObjectKind
  -> Precondition wm (AnyObject wm)
aKindOf k@(ObjectKind kName) = Precondition
  { preconditionName = return $ "is of kind " <> kName
  , checkPrecondition = \noun -> do
      noun `isKind` k
  }

intoThe ::
  LabelOptic' "into" A_Lens v (Thing wm)
  => ContainerEntity
  -> Precondition wm (Args wm v)
intoThe thing = Precondition
  { preconditionName = getThing thing >>= \t -> return $ "into the " <> display (t ^. #name)
  , checkPrecondition = \Args{variables} -> do
      return $ view #into variables `objectEquals` thing
  }

whenContainsSomething ::
  WMWithProperty wm Container
  => ContainerEntity
  -> Precondition wm v
whenContainsSomething theContainer = Precondition
  { preconditionName = getThing theContainer >>= \t -> return $ "when the " <> display (t ^. #name) <> " contains something"
  , checkPrecondition = const $ do
      c <- getContainer theContainer
      return $ (> 0) . ES.size $ c ^. #enclosing % #contents
  }