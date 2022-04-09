{-|
Module      : Yaifl.Activities.ChoosingNotableLocaleObjects
Description : The activity for choosing notable objects in the locale.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

module Yaifl.Activities.ChoosingNotableLocaleObjects
  ( choosingNotableLocaleObjectsImpl
  ) where

import Yaifl.Common
import Yaifl.Activities.Activity
import Solitude
import Yaifl.Properties.Property
import qualified Data.EnumMap as DEM
import qualified Data.EnumSet as DES
import Yaifl.Properties.Enclosing
import Yaifl.Objects.Object
import Yaifl.Properties.Query
import Yaifl.Logger
import Yaifl.Rulebooks.Rulebook
import Yaifl.Objects.Query


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