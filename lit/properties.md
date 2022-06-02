# Properties

properties

```haskell file=src/Yaifl/Properties/Property.hs
{-# LANGUAGE DefaultSignatures #-}

module Yaifl.Properties.Property 
  ( HasProperty(..)
  , WMHasProperty
  ) where

import Solitude ( Either(..), const, atraversal, eitherJoin, AffineTraversal' )
import Yaifl.Common ( WMObjSpecifics )

-- | A helper to define that a world model `wm` has a Property.
type WMHasProperty wm v = HasProperty (WMObjSpecifics wm) v

class HasProperty o v where
  default propertyL :: AffineTraversal' o v
  propertyL = atraversal Left const
  propertyL :: AffineTraversal' o v

instance (HasProperty a v, HasProperty b v) => HasProperty (Either a b) v where
  propertyL = propertyL `eitherJoin` propertyL

instance HasProperty () a
```


```haskell file=src/Yaifl/Properties/TH.hs
{-# LANGUAGE DataKinds #-}

module Yaifl.Properties.TH
(
    makeSpecificsWithout
  , makePropertyFunction
  , SpecificsFunctions(..)
  , makeDirections
) where

import Solitude
import Language.Haskell.Meta hiding (myDefaultParseMode)
import Data.Text (replace)
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Extension
import Language.Haskell.TH (Name, Q, Dec, nameBase)

data SpecificsFunctions =
  GetX
  | SetX
  | ModifyX
  deriving stock (Show, Eq, Enum, Ord, Generic, Bounded)

{-
getX :: HasProperty s X
=> HasID o
 => o
 -> State (World s) (Maybe X)
 getX = defaultPropertyGetter
-}

myDefaultParseMode :: ParseMode
myDefaultParseMode = defaultParseMode
  { parseFilename = []
  , baseLanguage = Haskell2010
  , extensions = map EnableExtension [DataKinds, ExplicitForAll, ScopedTypeVariables ]
  }

makeSpecificsWithout :: [SpecificsFunctions] -> Name -> Q [Dec]
makeSpecificsWithout l prop = do
  v <- mapM (makePropertyFunction prop) (universeSans l)
  return $ join v

makePropertyFunction :: Name -> SpecificsFunctions -> Q [Dec]
makePropertyFunction n sf = do
  return $ (case sf of
    GetX -> replaceTH 
      "getXSUBHERE :: MonadReader (World wm) m => Logger m => NoMissingObjects m => WMHasProperty wm XSUBHERE => MonadState (World wm) m => ObjectLike wm o => o -> m (Maybe XSUBHERE)\ngetXSUBHERE = defaultPropertyGetter"
    SetX -> replaceTH 
      "setXSUBHERE :: MonadReader (World wm) m => Logger m => WMHasProperty wm XSUBHERE => MonadState (World wm) m => HasID o => o-> XSUBHERE-> m ()\nsetXSUBHERE = defaultPropertySetter"
    ModifyX -> replaceTH 
      "modifyXSUBHERE :: MonadReader (World wm) m => Logger m => NoMissingObjects m => WMHasProperty wm XSUBHERE => MonadState (World wm) m => ObjectLike wm o => o -> (XSUBHERE -> XSUBHERE) -> m ()\nmodifyXSUBHERE = modifyProperty getXSUBHERE setXSUBHERE"
    ) (toText $ nameBase n)

replaceTH :: Text -> Text -> [Dec]
replaceTH y x = either (\x' -> [error $ toText x']) id (parseDecsWithMode myDefaultParseMode $ toString $ replace "XSUBHERE" x y)

makeDirections :: Bool -> [Text] -> Q [Dec]
makeDirections std dirs = do
  v <- mapM (\n -> do
    let replaceTH' y x = if std then replaceTH (replace "XSUBHERE2" "(injectDirection XSUBHERE)" y) x else replaceTH (replace "XSUBHERE2" "XSUBHERE" y) x
        r1 = replaceTH' "isXSUBHEREOf :: MonadWorld wm m => WMStdDirections wm => m Entity -> Entity -> m Entity\nisXSUBHEREOf = isDirectionFrom XSUBHERE2" n
        r2 = replaceTH' "isXSUBHEREOfOneWay :: MonadWorld wm m => WMStdDirections wm => m Entity -> Entity -> m Entity\nisXSUBHEREOfOneWay = isDirectionFromOneWay XSUBHERE2" n
    return $ r1 <> r2
    ) dirs
  return $ join v
```
