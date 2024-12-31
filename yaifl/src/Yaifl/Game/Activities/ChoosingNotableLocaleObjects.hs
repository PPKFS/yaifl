module Yaifl.Game.Activities.ChoosingNotableLocaleObjects
  ( choosingNotableLocaleObjectsImpl
  , WithChoosingNotableLocaleObjects
  ) where

import Yaifl.Prelude

import Yaifl.Model.Activity hiding (name)
import Yaifl.Core.Entity ( HasID(..) )
import Yaifl.Core.Store
import Yaifl.Core.Kinds.Object( Object(..) )
import Yaifl.Core.Kinds.Enclosing ( Enclosing(..) )
import Yaifl.Model.Rules.Rulebook (makeRule)
import qualified Data.EnumMap as DEM
import qualified Data.EnumSet as DES
import Breadcrumbs
import Yaifl.Game.Actions.Looking.Locale
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Core.ObjectLike
import Yaifl.Core.WorldModel
import Yaifl.Core.Query.Enclosing

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
          x' <- getThing x
          addAnnotation $ "Found a " <> display (x' ^. #name)
          return x') (DES.toList (contents encl))
        return (Just (Store $ DEM.fromList $ map (\x -> (getID x, LocaleInfo 5 x False)) l)))
  )]