{-# LANGUAGE DataKinds #-}

module Yaifl.Model.Properties.TH
(
    makeSpecificsWithout
  , makePropertyFunction
  , SpecificsFunctions(..)
  , makeDirections
) where

import Solitude
import Data.Text (replace)
import Language.Haskell.Exts.Extension ( Extension(..), KnownExtension(..), Language(..) )
import Language.Haskell.Exts.Parser ( defaultParseMode, ParseMode(..) )
import Language.Haskell.Meta ( parseDecsWithMode )
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
      "getXSUBHEREMaybe :: (CanBeAny wm o, WMWithProperty wm XSUBHERE) => o -> Maybe XSUBHERE\ngetXSUBHEREMaybe = defaultPropertyGetter"
    SetX -> replaceTH
      "setXSUBHERE :: (CanBeAny wm o, NoMissingObjects wm es, WMWithProperty wm XSUBHERE) => o -> XSUBHERE-> Eff es ()\nsetXSUBHERE = defaultPropertySetter"
    ModifyX -> replaceTH
      "modifyXSUBHERE :: (CanBeAny wm o, NoMissingObjects wm es, WMWithProperty wm XSUBHERE) => o -> (XSUBHERE -> XSUBHERE) -> Eff es ()\nmodifyXSUBHERE = modifyProperty getXSUBHEREMaybe setXSUBHERE"
    ) (toText $ nameBase n)

replaceTH :: Text -> Text -> [Dec]
replaceTH y x = either (\x' -> [error $ toText x']) id (parseDecsWithMode myDefaultParseMode $ toString $ replace "XSUBHERE" x y)

makeDirections :: Bool -> [Text] -> Q [Dec]
makeDirections std dirs = do
  v <- mapM (\n -> do
    let replaceTH' y x = if std then replaceTH (replace "XSUBHERE2" "(injectDirection XSUBHERE)" y) x else replaceTH (replace "XSUBHERE2" "XSUBHERE" y) x
        r1 = replaceTH' "isXSUBHEREOf :: NoMissingObjects wm es => WMStdDirections wm => Room wm -> Room wm -> Eff es ()\nisXSUBHEREOf = addDirectionFrom XSUBHERE2" n
        r2 = replaceTH' "isXSUBHEREOfOneWay :: NoMissingObjects wm es => WMStdDirections wm => Room wm -> Room wm -> Eff es ()\nisXSUBHEREOfOneWay = addDirectionFromOneWay XSUBHERE2" n
    return $ r1 <> r2
    ) dirs
  return $ join v
