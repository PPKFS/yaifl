{-|
Module      : Yaifl.Model.Properties.TH
Copyright   : (c) Avery 2023
License     : MIT
Maintainer  : ppkfs@outlook.com

Template Haskell generation of property queries (get, set, and modify) and directions (isDirectionOf
and isDirectionOfOneWay).
-}

module Yaifl.Model.TH
  ( makeSpecificsWithout
  , makePropertyFunction
  , SpecificsFunctions(..)
  , makeDirections
  , module Yaifl.Model.HasProperty
  , module Yaifl.Model.Kinds.AnyObject
  ) where

import Yaifl.Prelude
import Data.Text (replace)
import Language.Haskell.Exts.Extension ( Extension(..), KnownExtension(..), Language(..) )
import Language.Haskell.Exts.Parser ( defaultParseMode, ParseMode(..) )
import Language.Haskell.Meta ( parseDecsWithMode )
import Language.Haskell.TH (Name, Q, Dec, nameBase )
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.HasProperty
-- | The functions we *don't* want to autogenerate for a given property
-- because we want to do something special with them (e.g. see `Yaifl.Model.Kinds.Enclosing`
-- in `Yaifl.Model.Query` where @getEnclosingMaybe@ does something special with
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
  v <- mapM (makePropertyFunction prop) (universeSans l)
  return $ join v

-- | Generate one of @getPropMaybe@, @setProp@, and @modifyProp@.
makePropertyFunction :: Name -> SpecificsFunctions -> Q [Dec]
makePropertyFunction n sf = do
  return $ (case sf of
    GetX -> replaceTH
      "getXSUBHEREMaybe :: (CanBeAny wm o, WMWithProperty wm XSUBHERE) => o -> Maybe XSUBHERE\ngetXSUBHEREMaybe = defaultPropertyGetter"
    SetX -> replaceTH
      "setXSUBHERE :: (CanBeAny wm o, NoMissingObjects wm es, WMWithProperty wm XSUBHERE) => o -> XSUBHERE-> Eff es ()\nsetXSUBHERE = defaultPropertySetter"
    ModifyX -> replaceTH
      "modifyXSUBHERE :: (CanBeAny wm o, NoMissingObjects wm es, WMWithProperty wm XSUBHERE) => o -> (XSUBHERE -> XSUBHERE) -> Eff es ()\nmodifyXSUBHERE = modifyProperty getXSUBHEREMaybe setXSUBHERE"
    ) (toText $ nameBase n)

replaceTH :: Text -> Text -> [Dec]
replaceTH y x = either (\x' -> [error $ toText x']) id (parseDecsWithMode myDefaultParseMode $ toString $ replace "XSUBHERE" x y)

-- | Generate @isDirOf@ and @isDirOfOneWay@ for the base directions if @std@ is True, and for `Yaifl.Model.WorldModel.WMDirection` if False.
makeDirections ::
  Bool -- ^ Whether the directions should have an `Yaifl.Model.Kinds.Direction.injectDirection` wrapper if dealing with a supertype.
  -> [Text]
  -> Q [Dec]
makeDirections std dirs = do
  v <- mapM (\n -> do
    let replaceTH' y x = if std then replaceTH (replace "XSUBHERE2" "(injectDirection XSUBHERE)" y) x else replaceTH (replace "XSUBHERE2" "XSUBHERE" y) x
        r1 = replaceTH' "isXSUBHEREOf :: HasCallStack => NoMissingObjects wm es => WMStdDirections wm => RoomEntity -> RoomEntity -> Eff es ()\nisXSUBHEREOf = addDirectionFrom XSUBHERE2" n
        r2 = replaceTH' "isXSUBHEREOfOneWay :: NoMissingObjects wm es => WMStdDirections wm => RoomEntity -> RoomEntity -> Eff es ()\nisXSUBHEREOfOneWay = addDirectionFromOneWay XSUBHERE2" n
    return $ r1 <> r2
    ) dirs
  return $ join v
