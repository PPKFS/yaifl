module Yaifl.Preconditions
  ( forPlayer
  , forPlayer'
  , forKind

  ) where
import Yaifl.Prelude
import Yaifl.Rulebook
import Yaifl.Actions.Args
import Yaifl.Object.Kind
import Yaifl.Thing.Kind (Thing)
import Yaifl.Person.Query
import Yaifl.Metadata

forPlayer :: Precondition wm (Args wm v)
forPlayer = Precondition (pure "actor is the player") $ \v -> do
  p <- getPlayer
  pure $ p `objectEquals` (v ^. #source)

forPlayer' :: [Precondition wm (Args wm v)]
forPlayer' = [forPlayer]

forKind :: ObjectKind -> Precondition wm (Args wm (Thing wm))
forKind k = Precondition (pure $ "of kind " <> show k) $ \v -> variables v `isKind` k
