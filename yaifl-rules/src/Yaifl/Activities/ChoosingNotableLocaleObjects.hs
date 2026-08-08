module Yaifl.Activities.ChoosingNotableLocaleObjects
  ( choosingNotableLocaleObjectsImpl
  , WithChoosingNotableLocaleObjects
  ) where

import Yaifl.Prelude

import Breadcrumbs
import Yaifl.Activity hiding (name)
import Yaifl.Entity ( HasEntity(..) )
import Yaifl.AnyObject
import Yaifl.Enclosing.Kind ( Enclosing(..) )
import Yaifl.Object.Kind( Object(..) )
import Yaifl.ObjectLike
import Yaifl.Enclosing.Query
import Yaifl.Rulebook
import Yaifl.Store
import Yaifl.Locale
import qualified Data.EnumMap as DEM
import qualified Data.EnumSet as DES
import Yaifl.Property.Has
import Yaifl.Region.Query (getEnclosingRegions)
import Yaifl.Thing.Query
import Yaifl.Metadata
import Yaifl.Region.Kind

type WithChoosingNotableLocaleObjects wm = (WithActivity "choosingNotableLocaleObjects" wm () (AnyObject wm) (LocalePriorities wm))

choosingNotableLocaleObjectsImpl ::
  forall wm.
  WMWithProperty wm Enclosing
  => Activity wm () (AnyObject wm) (LocalePriorities wm)
choosingNotableLocaleObjectsImpl = makeActivity "Choosing notable locale objects" [makeRule "" []
  (\v -> do
    case getEnclosingMaybe v of
      Nothing -> (do
        addAnnotation $ "Tried to choose notable locale objects from " <> display (v ^. #name) <> " which doesn't enclose."
        return Nothing)
      Just encl -> do
        l <- mapM (\x -> do
          x' <- getThing x
          addAnnotation $ "Found a " <> display (x' ^. #name)
          return x') (DES.toList (contents encl))
        loc <- asThingOrRoom getLocation pure v
        rs <- getEnclosingRegions loc
        let regionBackdropIds = mconcat $ map (toList . backdrops) rs
        everywhereBackdropIds <- toList <$> use @(Metadata wm) #everywhereBackdrops
        mbBackdrops <- mapM getThing (regionBackdropIds <> everywhereBackdropIds)
        return (Just (Store $ DEM.fromList $ map (\x -> (getEntity x, LocaleInfo 5 x False)) (mbBackdrops <> l) ))
  )]