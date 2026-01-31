module Yaifl.Preconditions
  ( forPlayer
  , forPlayer'
  , forKind
  , theObject
  , theObject'
  , whenIn
  , whenPlayerIsIn
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

whenPlayerIsIn ::
  ObjectLike wm e
  => WMWithProperty wm MultiLocated
  => IsEnclosing e
  => e
  -> Precondition wm a
whenPlayerIsIn e = Precondition
  { preconditionName = do
      e' <- getObject e
      pure $ "when in the location " <> display (e' ^. #name)
  , checkPrecondition = const $ do
      hierarchy <- getPlayer' >>= getContainingHierarchies
      pure $ any (elem (getEnclosingEntity e)) hierarchy
  }

aKindOf ::
  ObjectKind
  -> Precondition wm (AnyObject wm)
aKindOf k@(ObjectKind kName) = Precondition
  { preconditionName = return $ "is of kind " <> kName
  , checkPrecondition = \noun -> do
      noun `isKind` k
  }
