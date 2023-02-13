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
  , WithChoosingNotableLocaleObjects
  ) where

import Solitude

import Yaifl.Core.Actions.Activity hiding (name)
import Yaifl.Core.Entity ( Store(..), HasID(..) )
import Yaifl.Core.Object ( Object(..), AnyObject )
import Yaifl.Core.Objects.Query ( getObject )
import Yaifl.Core.Properties.Enclosing ( Enclosing(..) )
import Yaifl.Core.Properties.Has ( WMHasProperty )
import Yaifl.Core.Properties.Query ( getEnclosing )
import Yaifl.Core.Rulebooks.Rule (makeRule)
import qualified Data.EnumMap as DEM
import qualified Data.EnumSet as DES
import Breadcrumbs
import Text.Interpolation.Nyan

type WithChoosingNotableLocaleObjects wm = (WithActivity "choosingNotableLocaleObjects" wm (AnyObject wm) (LocalePriorities wm))
choosingNotableLocaleObjectsImpl ::
  WMHasProperty wm Enclosing
  => Activity wm (AnyObject wm) (LocalePriorities wm)
choosingNotableLocaleObjectsImpl = makeActivity "Choosing notable locale objects" [makeRule ""
  (\v -> do
    e' <- getEnclosing v
    case e' of
      Nothing -> (do
        addAnnotation [int|t|Tried to choose notable locale objects from #{name v} which doesn't enclose.|]
        return Nothing)
      Just encl -> (do
        l <- mapM (\x -> do
          x' <- getObject x
          addAnnotation $ "Found a " <> name x'
          return x') (DES.toList (contents encl))
        return (Just (Store $ DEM.fromList $ map (\x -> (getID x, LocaleInfo 5 x False)) l)))
  )]