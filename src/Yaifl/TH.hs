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
    | GetX'
    | SetX'
    | ModifyX
    | ModifyX'
    deriving stock (Show, Eq, Enum, Ord, Generic, Bounded)

{-
getX :: HasProperty s X
=> ObjectLike o
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
    o <- newName "o"
    s <- newName "s"
    v <- newName "v"

    return $ (case sf of
        GetX -> replaceTH "getXSUBHERE :: HasProperty s XSUBHERE => ObjectLike o => o -> State (World s) (Maybe XSUBHERE)\ngetXSUBHERE = defaultPropertyGetter"
        GetX' -> replaceTH "getXSUBHERE':: HasProperty s XSUBHERE => ObjectLike o => o -> World s -> Maybe XSUBHERE\ngetXSUBHERE' = evalState . getXSUBHERE"
        SetX -> replaceTH "setXSUBHERE :: HasProperty s XSUBHERE => ObjectLike o => o-> XSUBHERE-> State (World s) ()\nsetXSUBHERE = defaultPropertySetter"
        SetX' -> replaceTH "setXSUBHERE' :: HasProperty s XSUBHERE => ObjectLike o => o-> XSUBHERE -> World s -> World s\nsetXSUBHERE' o e = execState (setXSUBHERE o e)"
        ModifyX -> replaceTH "modifyXSUBHERE :: HasProperty s XSUBHERE => ObjectLike o=> o-> (XSUBHERE -> XSUBHERE) -> State (World s) ()\nmodifyXSUBHERE = modifyProperty getXSUBHERE setXSUBHERE"
        ModifyX' -> replaceTH "--" --replaceTH "modifyXSUBHERE' :: HasProperty s XSUBHERE => ObjectLike o=> o-> (XSUBHERE -> XSUBHERE)-> World s -> World s\nmodifyXSUBHERE' = evalState . (modifyProperty getXSUBHERE setXSUBHERE)"
        ) (toText $ nameBase n)

replaceTH :: Text -> Text -> [Dec]
replaceTH y x = fromRight [] (parseDecs $ toString $ replace "XSUBHERE" x y)