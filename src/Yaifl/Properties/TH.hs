{-|
Module      : Yaifl.Actions.Action
Description : An action is a verb that is carried out by the player (or an NPC).
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}


module Yaifl.Properties.TH
(
    makeSpecificsWithout
  , makePropertyFunction
  , SpecificsFunctions(..)
) where

import Solitude
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
        GetX -> replaceTH 
            "getXSUBHERE :: MonadReader (World wm) m => Logger m => NoMissingObjects m => WMHasProperty wm XSUBHERE => MonadState (World wm) m => ObjectLike wm o => o -> m (Maybe XSUBHERE)\ngetXSUBHERE = defaultPropertyGetter"
        SetX -> replaceTH 
            "setXSUBHERE :: MonadReader (World wm) m => Logger m => WMHasProperty wm XSUBHERE => MonadState (World wm) m => HasID o => o-> XSUBHERE-> m ()\nsetXSUBHERE = defaultPropertySetter"
        ModifyX -> replaceTH 
            "modifyXSUBHERE :: MonadReader (World wm) m => Logger m => NoMissingObjects m => WMHasProperty wm XSUBHERE => MonadState (World wm) m => ObjectLike wm o => o -> (XSUBHERE -> XSUBHERE) -> m ()\nmodifyXSUBHERE = modifyProperty getXSUBHERE setXSUBHERE"
        ) (toText $ nameBase n)

replaceTH :: Text -> Text -> [Dec]
replaceTH y x = fromRight [] (parseDecs $ toString $ replace "XSUBHERE" x y)