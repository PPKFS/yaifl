{-|
Module      : Yaifl.Activities.ChoosingNotableLocaleObjects
Description : The activity for choosing notable objects in the locale.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

module Yaifl.Core.Activities.ChoosingNotableLocaleObjects
  ( choosingNotableLocaleObjectsImpl
  ) where

import Yaifl.Core.Common
import Yaifl.Core.Activities.Activity
import Solitude
import Yaifl.Core.Properties.Property
import qualified Data.EnumMap as DEM
import qualified Data.EnumSet as DES
import Yaifl.Core.Properties.Enclosing
import Yaifl.Core.Objects.Object
import Yaifl.Core.Properties.Query
import Yaifl.Core.Logger
import Yaifl.Core.Rulebooks.Rulebook
import Yaifl.Core.Objects.Query


choosingNotableLocaleObjectsImpl :: 
  WMHasProperty s Enclosing
  => Activity s (AnyObject s) (LocalePriorities s)
choosingNotableLocaleObjectsImpl = makeActivity "Choosing notable locale objects" $ makeRule "" 
  (\v -> do
    n <- objectName v
    e' <- getEnclosing v 
    case e' of
      Nothing -> (do
        warn $ bformat ("Tried to choose notable locale objects from " 
          %! stext %! " which that doesn't enclose ") n
        return Nothing)
      Just encl -> (do
        l <- mapM (\x -> do
          x' <- getObject x
          n' <- objectName x'
          debug $ bformat ("Found a " %! stext) n'
          return x') (DES.toList (_enclosingContains encl))
        return (Just (Store $ DEM.fromList $ map (\x -> (getID x, LocaleInfo 5 x False)) l)))
  )