module Yaifl.TH
(
    makeSpecificsWithout
  , makePropertyFunction
  , SpecificsFunctions(..)
) where

import Yaifl.Prelude
import Language.Haskell.TH
import Language.Haskell.Meta
import Data.Text (replace)

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

makeSpecificsWithout :: [SpecificsFunctions] -> Name -> Q [Dec]
makeSpecificsWithout l prop = do
    v <- mapM (makePropertyFunction prop) (universeSans l)
    return $ join v

makePropertyFunction :: Name -> SpecificsFunctions -> Q [Dec]
makePropertyFunction n sf = do
    return $ (case sf of
        GetX -> replaceTH "getXSUBHERE :: HasProperty s XSUBHERE => MonadWorld s m => ObjectLike s o => o -> m (Maybe XSUBHERE)\ngetXSUBHERE = defaultPropertyGetter"
        SetX -> replaceTH "setXSUBHERE :: HasProperty s XSUBHERE => MonadWorld s m => HasID o => o-> XSUBHERE-> m ()\nsetXSUBHERE = defaultPropertySetter"
        ModifyX -> replaceTH "modifyXSUBHERE :: HasProperty s XSUBHERE => MonadWorld s m => ObjectLike s o => o-> (XSUBHERE -> XSUBHERE) -> m ()\nmodifyXSUBHERE = modifyProperty getXSUBHERE setXSUBHERE"
        ) (toText $ nameBase n)

replaceTH :: Text -> Text -> [Dec]
replaceTH y x = fromRight [] (parseDecs $ toString $ replace "XSUBHERE" x y)