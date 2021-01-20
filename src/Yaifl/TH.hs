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
      dataDef = DataD [] worldType [] Nothing [records] [DerivClause Nothing [ConT ''Show]]
      makeRecord t = (mkLensName t, bangDef, 
        ConT (mkName "Store") `AppT` ConT t)
      records = RecC worldType (map makeRecord componentStores) -- <> 
      blankWorldCtr = FunD (blankName worldType) [Clause [] (NormalB (iterExpr expr)) []]
      expr = AppE (ConE worldType) (VarE $ mkName "emptyStore")
      iterExpr = foldr (.) id $ replicate (length componentStores - 1) (\x -> AppE x (VarE $ mkName "emptyStore"))
  return [dataDef, blankWorldCtr]

blankName :: Name -> Name
blankName n = mkName $ "blank" <> nameBase n

--gameinfo :: TH.Type -> (Name, Bang, TH.Type)
--gameinfo w = (mkName "_worldGameInfo", bangDef, AppT (ConT $ mkName "GameInfo") w)