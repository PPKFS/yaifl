module Yaifl.Activities.ChoosingNotableLocaleObjects
  ( choosingNotableLocaleObjectsImpl
  , WithChoosingNotableLocaleObjects
  ) where

import Solitude

import Yaifl.Activities.Activity hiding (name)
import Yaifl.Model.Entity ( Store(..), HasID(..) )
import Yaifl.Model.Object( Object(..), AnyObject )
import Yaifl.Model.Objects.Query ( getObject )
import Yaifl.Model.Properties.Enclosing ( Enclosing(..) )
import Yaifl.Model.Properties.Has ( WMWithProperty )
import Yaifl.Model.Properties.Query ( getEnclosingMaybe )
import Yaifl.Rules.Rule (makeRule)
import qualified Data.EnumMap as DEM
import qualified Data.EnumSet as DES
import Breadcrumbs
import Data.Text.Display
import Yaifl.Actions.Looking.Locale

type WithChoosingNotableLocaleObjects wm = (WithActivity "choosingNotableLocaleObjects" wm (AnyObject wm) (LocalePriorities wm))
choosingNotableLocaleObjectsImpl ::
  WMWithProperty wm Enclosing
  => Activity wm (AnyObject wm) (LocalePriorities wm)
choosingNotableLocaleObjectsImpl = makeActivity "Choosing notable locale objects" [makeRule "" []
  (\v -> do
    let e' = getEnclosingMaybe v
    case e' of
      Nothing -> (do
        addAnnotation $ "Tried to choose notable locale objects from " <> display (v ^. #name) <> " which doesn't enclose."
        return Nothing)
      Just encl -> (do
        l <- mapM (\x -> do
          x' <- getObject x
          addAnnotation $ "Found a " <> display (x' ^. #name)
          return x') (DES.toList (contents encl))
        return (Just (Store $ DEM.fromList $ map (\x -> (getID x, LocaleInfo 5 x False)) l)))
  )]