{-# LANGUAGE TemplateHaskell #-}
module Yaifl.TH
       (
           makeWorld
       ) where

import Language.Haskell.TH as TH
import Data.Char
import Relude

data StoreType = Unique Name | Map Name

getStoreName :: StoreType -> Name
getStoreName (Unique n) = n
getStoreName (Map n) = n

bangDef :: Bang
bangDef = Bang NoSourceUnpackedness NoSourceStrictness

replaceFirst :: [a] -> (a -> a) -> [a]
replaceFirst [] _ = []
replaceFirst (x:xs) f = f x : xs

mkLensName :: Name -> Name
mkLensName t = mkName $ "_" <> replaceFirst (nameBase t <> "Store") toLower

makeWorld :: Text -> [Name] -> Q [Dec]
makeWorld typeName componentStores = do
  let worldType = mkName $ toString typeName
  stores <- mapM (makeStoreDatatypes worldType) componentStores
  let dataDef = DataD [] worldType [] Nothing [records] [DerivClause Nothing [ConT ''Show]]
      makeRecord (t, (e, _)) = (mkLensName t, bangDef, AppT (ConT (mkName "Store")) e)
      records = RecC worldType (zipWith (curry makeRecord) componentStores stores) -- <> 
      blankWorldCtr = FunD (blankName worldType) [Clause [] (NormalB (iterExpr expr)) []]
      expr = AppE (ConE worldType) (VarE $ mkName "emptyStore")
      iterExpr = foldr (.) id $ replicate (length componentStores - 1) (\x -> AppE x (VarE $ mkName "emptyStore"))
  return [dataDef, blankWorldCtr]
makeStoreDatatypes :: Name -> Name -> Q (TH.Type, Bool)
makeStoreDatatypes param a = do
  TyConI tyCon <- reify a
  (tyConName, tyVars) <- case tyCon of
    DataD _ nm tyVars _ _ _  -> return (nm, tyVars)
    NewtypeD _ nm tyVars _ _ _ -> return (nm, tyVars)
    _ -> fail "deriveFunctor: tyCon may not be a type synonym."
    -- if we are dealing with Foo x, then construct a forced w type
  let (v, fl) = if null tyVars then (ConT a, False) else (AppT (ConT a) (ConT param), True)--mkName "w" 
  return (v, fl)

blankName :: Name -> Name
blankName n = mkName $ "blank" <> nameBase n

--gameinfo :: TH.Type -> (Name, Bang, TH.Type)
--gameinfo w = (mkName "_worldGameInfo", bangDef, AppT (ConT $ mkName "GameInfo") w)