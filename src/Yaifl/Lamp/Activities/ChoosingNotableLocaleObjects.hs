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

import Solitude

import Yaifl.Core.Actions.Activity
import Yaifl.Core.AdaptiveText.Eval
import Yaifl.Core.Entity ( Store(..), HasID(..) )
import Yaifl.Core.Logger ( debug, warn )
import Yaifl.Core.Object ( Object(..), AnyObject )
import Yaifl.Core.Objects.Query ( getObject )
import Yaifl.Core.Properties.Enclosing ( Enclosing(..) )
import Yaifl.Core.Properties.Has ( WMHasProperty )
import Yaifl.Core.Properties.Query ( getEnclosing )
import Yaifl.Core.Rulebooks.Rule (makeRule)
import qualified Data.EnumMap as DEM
import qualified Data.EnumSet as DES

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
          xn <- evalName x'
          debug $ "Found a " <> xn
          return x') (DES.toList (_enclosingContains encl))
        return (Just (Store $ DEM.fromList $ map (\x -> (getID x, LocaleInfo 5 x False)) l)))
  )