-- ~\~ language=Haskell filename=src/Yaifl/Core/Properties/TH.hs
-- ~\~ begin <<lit/properties/getsetmodify.md|src/Yaifl/Core/Properties/TH.hs>>[0] project://lit/properties/getsetmodify.md:5
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Properties.TH
(
    makeSpecificsWithout
  , makePropertyFunction
  , SpecificsFunctions(..)
  , makeDirections
) where


import Language.Haskell.Meta hiding (myDefaultParseMode)
import Data.Text (replace)
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Extension
import Language.Haskell.TH (Name, Q, Dec, nameBase, mkName)
import Language.Haskell.TH.Quote

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

makePropertyFunction :: Name -> SpecificsFunctions -> Q Dec
makePropertyFunction n sf = do
  let getName = mkName "getBlah"
  return $ case sf of
    GetX -> [d| 
      getName :: $(ObjectLookup) wm :> es
        => Eff es (Maybe $(nameBase n))
        $(getName) = error ""
      |]
    _ -> error ""
    {- -- "getXSUBHERE :: MonadReader (World wm) m => Logger m => NoMissingObjects m => WMHasProperty wm XSUBHERE => MonadState (World wm) m => ObjectLike wm o => o -> m (Maybe XSUBHERE)\ngetXSUBHERE = defaultPropertyGetter"
    SetX -> replaceTH 
      "setXSUBHERE :: MonadReader (World wm) m => Logger m => WMHasProperty wm XSUBHERE => MonadState (World wm) m => HasID o => o-> XSUBHERE-> m ()\nsetXSUBHERE = defaultPropertySetter"
    ModifyX -> replaceTH 
      "modifyXSUBHERE :: MonadReader (World wm) m => Logger m => NoMissingObjects m => WMHasProperty wm XSUBHERE => MonadState (World wm) m => ObjectLike wm o => o -> (XSUBHERE -> XSUBHERE) -> m ()\nmodifyXSUBHERE = modifyProperty getXSUBHERE setXSUBHERE"
    ) (toText $ nameBase n) -}

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
-- ~\~ end
