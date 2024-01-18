module Yaifl.Game.Activities.ChoosingNotableLocaleObjects
  ( choosingNotableLocaleObjectsImpl
  , WithChoosingNotableLocaleObjects
  ) where

import Solitude

import Yaifl.Model.Activity hiding (name)
import Yaifl.Model.Entity ( HasID(..) )
import Yaifl.Model.Store
import Yaifl.Model.Kinds.Object( Object(..) )
import Yaifl.Model.Kinds.Enclosing ( Enclosing(..) )
import Yaifl.Model.HasProperty ( WMWithProperty )
import Yaifl.Model.Query ( getEnclosingMaybe )
import Yaifl.Model.Rules.Rulebook (makeRule)
import qualified Data.EnumMap as DEM
import qualified Data.EnumSet as DES
import Breadcrumbs
import Data.Text.Display
import Yaifl.Game.Actions.Looking.Locale
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.ObjectLike

type WithChoosingNotableLocaleObjects wm = (WithActivity "choosingNotableLocaleObjects" wm () (AnyObject wm) (LocalePriorities wm))

choosingNotableLocaleObjectsImpl ::
  WMWithProperty wm Enclosing
  => Activity wm () (AnyObject wm) (LocalePriorities wm)
choosingNotableLocaleObjectsImpl = makeActivity "Choosing notable locale objects" [makeRule "" []
  (\v -> do
    case getEnclosingMaybe v of
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