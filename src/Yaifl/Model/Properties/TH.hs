{-# LANGUAGE DataKinds #-}

module Yaifl.Model.Properties.TH
(
    makeSpecificsWithout
  , makePropertyFunction
  , SpecificsFunctions(..)
  , makeDirections
) where

import Solitude
import Language.Haskell.Meta ( parseDecsWithMode )
import Data.Text (replace)
import Language.Haskell.Exts.Parser ( defaultParseMode, ParseMode(..) )
import Language.Haskell.Exts.Extension ( Extension(..), KnownExtension(..), Language(..) )
import Language.Haskell.TH (Name, Q, Dec, nameBase)

data SpecificsFunctions =
  GetX
  | SetX
  | ModifyX
  deriving stock (Show, Eq, Enum, Ord, Generic, Bounded)

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
      "getXSUBHERE :: (NoMissingObjects wm es, WMHasProperty wm XSUBHERE, ObjectLike wm o) => o -> Eff es (Maybe XSUBHERE)\ngetXSUBHERE = defaultPropertyGetter"
    SetX -> replaceTH
      "setXSUBHERE :: (NoMissingObjects wm es, WMHasProperty wm XSUBHERE, ObjectLike wm o) => o -> XSUBHERE-> Eff es ()\nsetXSUBHERE = defaultPropertySetter"
    ModifyX -> replaceTH
      "modifyXSUBHERE :: (NoMissingObjects wm es, WMHasProperty wm XSUBHERE, ObjectLike wm o) => o -> (XSUBHERE -> XSUBHERE) -> Eff es ()\nmodifyXSUBHERE = modifyProperty getXSUBHERE setXSUBHERE"
    ) (toText $ nameBase n)

replaceTH :: Text -> Text -> [Dec]
replaceTH y x = either (\x' -> [error $ toText x']) id (parseDecsWithMode myDefaultParseMode $ toString $ replace "XSUBHERE" x y)

makeDirections :: Bool -> [Text] -> Q [Dec]
makeDirections std dirs = do
  v <- mapM (\n -> do
    let replaceTH' y x = if std then replaceTH (replace "XSUBHERE2" "(injectDirection XSUBHERE)" y) x else replaceTH (replace "XSUBHERE2" "XSUBHERE" y) x
        r1 = replaceTH' "isXSUBHEREOf :: (Breadcrumbs :> es, ObjectQuery wm es, State (Metadata) :> es, Display (WMSayable wm)) => WMStdDirections wm => Room wm -> Room wm -> Eff es ()\nisXSUBHEREOf = addDirectionFrom XSUBHERE2" n
        r2 = replaceTH' "isXSUBHEREOfOneWay :: (Breadcrumbs :> es, ObjectQuery wm es, State (Metadata) :> es, Display (WMSayable wm)) => WMStdDirections wm => Room wm -> Room wm -> Eff es ()\nisXSUBHEREOfOneWay = addDirectionFromOneWay XSUBHERE2" n
    return $ r1 <> r2
    ) dirs
  return $ join v
