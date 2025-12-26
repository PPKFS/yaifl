{-|
Module      : Yaifl.TH
Copyright   : (c) Avery 2023-2025
License     : MIT
Maintainer  : ppkfs@outlook.com

Template Haskell generation of property queries (get, set, and modify) and directions (isDirectionOf
and isDirectionOfOneWay).

At some point this really should be done properly so we can drop the src-exts dependency.
-}

module Yaifl.TH
  ( makeSpecificsWithout
  , makePropertyFunction
  , makeGetMaybe
  , makeModify
  , SpecificsFunctions(..)
  , makeDirections
  , module Yaifl.Property.Has
  , module Yaifl.Property.Query
  , module Yaifl.AnyObject
  , module Yaifl.WorldModel
  ) where

import Data.Text (replace)
import Language.Haskell.Exts.Extension ( Extension(..), KnownExtension(..), Language(..) )
import Language.Haskell.Exts.Parser ( defaultParseMode, ParseMode(..) )
import Language.Haskell.Meta ( parseDecsWithMode )
import Language.Haskell.TH (Name, Q, Dec, nameBase )
import Yaifl.Property.Has
import Yaifl.WorldModel
import Yaifl.AnyObject
import Yaifl.Prelude
import Yaifl.Property.Query
-- | The functions we *don't* want to autogenerate for a given property
-- because we want to do something special with them (e.g. see `Yaifl.Enclosing.Kind`
-- in `Yaifl.Object.Query` where @getEnclosingMaybe@ does something special with
-- rooms).
data SpecificsFunctions =
  GetX
  | SetX
  | ModifyX
  deriving stock (Show, Eq, Enum, Ord, Generic, Bounded)

myDefaultParseMode :: ParseMode
myDefaultParseMode = defaultParseMode
  { parseFilename = []
  , baseLanguage = Haskell2010
  , extensions = map EnableExtension [DataKinds, ExplicitForAll, ScopedTypeVariables, FlexibleContexts ]
  }

-- | Generate 0-3 of @getPropMaybe@, @setProp@, and @modifyProp@.
makeSpecificsWithout ::
  [SpecificsFunctions]
  -> Name
  -> Q [Dec]
makeSpecificsWithout l prop = do
  v <- mapM (`makePropertyFunction` prop) (universeSans l)
  return $ join v

makeGetMaybe ::
  Name
  -> Q [Dec]
makeGetMaybe = makePropertyFunction GetX

makeModify ::
  Name
  -> Q [Dec]
makeModify n = do
  s <- makePropertyFunction SetX n
  m <- makePropertyFunction ModifyX n
  return $ s <> m

-- | Generate one of @getPropMaybe@, @setProp@, and @modifyProp@.
makePropertyFunction :: SpecificsFunctions -> Name -> Q [Dec]
makePropertyFunction sf n = do
  return $ (case sf of
    GetX -> replaceTH
      "getXSUBHEREMaybe :: (CanBeAny wm o, WMWithProperty wm XSUBHERE) => o -> Maybe XSUBHERE\ngetXSUBHEREMaybe = defaultPropertyGetter"
    SetX -> replaceTH
      "setXSUBHERE :: (CanBeAny wm o, WithoutMissingObjects wm es, WMWithProperty wm XSUBHERE) => o -> XSUBHERE-> Eff es ()\nsetXSUBHERE = defaultPropertySetter"
    ModifyX -> replaceTH
      "modifyXSUBHERE :: (CanBeAny wm o, WithoutMissingObjects wm es, WMWithProperty wm XSUBHERE) => o -> (XSUBHERE -> XSUBHERE) -> Eff es ()\nmodifyXSUBHERE = modifyProperty getXSUBHEREMaybe setXSUBHERE"
    ) (toText $ nameBase n)

replaceTH :: Text -> Text -> [Dec]
replaceTH y x = either (\x' -> [error $ toText x']) id (parseDecsWithMode myDefaultParseMode $ toString $ replace "XSUBHERE" x y)

-- | Generate @isDirOf@ and @isDirOfOneWay@ for the base directions if @std@ is True, and for `Yaifl.WorldModel.WMDirection` if False.
makeDirections ::
  Bool -- ^ Whether the directions should have an `Yaifl.Direction.Kind.injectDirection` wrapper if dealing with a supertype.
  -> [Text]
  -> Q [Dec]
makeDirections std dirs = do
  v <- mapM (\n -> do
    let replaceTH' y x = if std then replaceTH (replace "XSUBHERE2" "(injectDirection XSUBHERE)" y) x else replaceTH (replace "XSUBHERE2" "XSUBHERE" y) x
        r1 = replaceTH' "isXSUBHEREOf :: HasCallStack => WithoutMissingObjects wm es => WMStdDirections wm => RoomEntity -> RoomEntity -> Eff es ()\nisXSUBHEREOf = addDirectionFrom XSUBHERE2" n
        r2 = replaceTH' "isXSUBHEREOfOneWay :: WithoutMissingObjects wm es => WMStdDirections wm => RoomEntity -> RoomEntity -> Eff es ()\nisXSUBHEREOfOneWay = addDirectionFromOneWay XSUBHERE2" n
    return $ r1 <> r2
    ) dirs
  return $ join v
