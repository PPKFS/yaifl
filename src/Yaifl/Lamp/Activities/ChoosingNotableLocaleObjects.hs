{-|
Module      : Yaifl.Activities.ChoosingNotableLocaleObjects
Description : The activity for choosing notable objects in the locale.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

module Yaifl.Lamp.Activities.ChoosingNotableLocaleObjects
  ( choosingNotableLocaleObjectsImpl
  ) where

import Yaifl.Core.Common
import Yaifl.Core.Actions.Activity

import Yaifl.Core.Properties.Property
import qualified Data.EnumMap as DEM
import qualified Data.EnumSet as DES
import Yaifl.Core.Properties.Enclosing
import Yaifl.Core.Objects.Object
import Yaifl.Core.Properties.Query
import Yaifl.Core.Logger
import Yaifl.Core.Objects.Query
import Yaifl.Core.Rulebooks.Rule (makeRule)
import Text.Interpolation.Nyan


choosingNotableLocaleObjectsImpl :: 
  WMHasProperty wm Enclosing
  => Activity wm (AnyObject wm) (LocalePriorities wm)
choosingNotableLocaleObjectsImpl = makeActivity "Choosing notable locale objects" $ makeRule "" 
  (\v -> do
    e' <- getEnclosing v 
    case e' of
      Nothing -> (do
        warn [int|t|Tried to choose notable locale objects from #{_objName v} which doesn't enclose.|]
        return Nothing)
      Just encl -> (do
        l <- mapM (\x -> do
          x' <- getObject x
          debug $ "Found a " <> _objName x'
          return x') (DES.toList (_enclosingContains encl))
        return (Just (Store $ DEM.fromList $ map (\x -> (getID x, LocaleInfo 5 x False)) l)))
  )