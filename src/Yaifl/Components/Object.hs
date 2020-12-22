module Yaifl.Components.Object
    ( Name
    , Description(..)
    , Object(..)
    , objectComponent
    , makeObject
    , HasWorld
    , HasComponents
    , nameOf
    , HasName
    )
where

import           Yaifl.Common2
import           Yaifl.Say2
import           Yaifl.Prelude
import           Yaifl.World
import qualified Text.Show

-- the printed name of something
type Name = Text

data Description = PlainDescription Text | DynamicDescription (forall w. w -> Entity -> Text)

instance IsString Description where
    fromString = PlainDescription . fromString

-- | also for overloadedstrings
instance Semigroup Description where
    (<>) (PlainDescription e) (PlainDescription e2) =
        PlainDescription (e <> e2)
    (<>) (PlainDescription e) (DynamicDescription e2) =
        DynamicDescription (\w e1 -> e <> e2 w e1)
    (<>) (DynamicDescription e2) (PlainDescription e) =
        DynamicDescription (\w e1 -> e2 w e1 <> e)
    (<>) (DynamicDescription e) (DynamicDescription e2) =
        DynamicDescription (\w e1 -> e w e1 <> e2 w e1)
-- | we define a show instance just for debug purposes.
instance Show Description where
    show (PlainDescription   t) = show t
    show (DynamicDescription _) = "dynamic description"

data Object = Object
    { _name        :: Name
    , _description :: Description
    , _objectID    :: Entity
    , _objectType  :: Text
    }
    deriving Show
makeLenses ''Object

class HasName a where
    nameOf :: (HasWorld w '[Object] r) => a -> Sem r Name

instance HasName Entity where
    nameOf e = do
        _name <$> getComponent' objectComponent e

type HasWorld w c r
    = (Members (SemWorldList w) r, HasComponents w c)

type family HasComponents w (constraints :: [Type]) :: Constraint where
  HasComponents w '[] = (HasStore w Object)
  HasComponents w (x ': xs) = (HasStore w x, HasComponents w xs)

objectComponent :: Proxy Object
objectComponent = Proxy

makeObject :: (WithLogging r, HasWorld w '[Object] r) => Text -> Description -> Text -> Sem r Entity
makeObject n d t = do
    e <- newEntity
    addComponent e (Object n d e t)
    logMsg Info $ "Made a new " <> t <> " with ID " <> show e <> " called " <> n
    return e