{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Yaifl.Object.Create
  ( makeNameImproper
  , makeNameProper
  , done
  , AddObjects
  , makeObject
  , addObject
  ) where

import Yaifl.Prelude
import Breadcrumbs

import Yaifl.Metadata
import Yaifl.Object.Kind
import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.Move ( move )
import Yaifl.ObjectLike
import Yaifl.Room.Kind ( tagRoomEntity )
import Yaifl.Thing.Kind
import Yaifl.Enclosing.Kind ( Enclosing )
import Yaifl.WorldModel

import qualified Data.Set as S
import Data.Char (isUpper)
import qualified Data.Text as T
import Yaifl.Tag (tagObject)
import Yaifl.AnyObject
import Yaifl.Property.Has
import Yaifl.Text.SayableValue
import Yaifl.Effects.RuleEffects
import Yaifl.ObjectSpecifics
import Yaifl.Direction.Kind
import Yaifl.MultiLocated.Kind

done = defaults


-- | Type synonym for adding new objects.
type AddObjects wm es = (
  Display (WMText wm)
  , IsString (WMText wm)
  , Pointed (WMObjSpecifics wm)
  , Pointed (WMRegionData wm)
  , Pointed (WMRoomData wm)
  , Pointed (WMThingData wm)
  , RuleEffects wm es
  , SayableValue (WMText wm) wm
  , WMHasObjSpecifics wm
  , WMStdDirections wm
  , WMWithProperty wm Enclosing
  , WMWithProperty wm MultiLocated
  )

makeObject ::
  AddObjects wm es
  => Pointed s
  => WMText wm -- ^ Name.
  -> WMText wm -- ^ Description.
  -> ObjectKind
  -> Bool
  -> Maybe s -- ^ Object details.
  -> d
  -> Eff es (Entity, Object wm d s)
makeObject n d ty isT specifics details = do
  e <- generateEntity isT
  t <- getGlobalTime
  let shownName = display n
  return (e, Object n Nothing PubliclyNamed Nothing S.empty SingularNamed
    (if not (T.null shownName) && isUpper (T.head shownName) then Proper else Improper) d e ty t t (fromMaybe identityElement specifics) details)

addObject ::
  forall wm s d es.
  AddObjects wm es
  => Pointed s
  => (Object wm d s -> Eff es ())
  -> WMText wm -- ^ Name.
  -> WMText wm -- ^ Description.
  -> ObjectKind
  -> Bool
  -> Maybe s
  -> d
  -> Maybe EnclosingEntity
  -> Eff es (Object wm d s)
addObject updWorld n d ty isT specifics details mbLocation =
  withSpan' ("new " <> if isT then "thing" else "room") (display n) $ do
    (e, obj) <- makeObject n d ty isT specifics details
    addAnnotation "object created"
    updWorld obj
    addAnnotation "object added to world"
    lastRoomE <- use #previousRoom
    tickGlobalTime
    do
      obj' <- getObject e
      lastRoom <- getRoom lastRoomE
      asThingOrRoom
        (\t -> do
          case mbLocation of
            Nothing -> move t lastRoom >> pass
            Just loc -> do
              encLoc <- getObject loc
              asThingOrRoom
                (void . move @(EnclosingThing wm) @_ @_ t . tagObject @EnclosingEntity @EnclosingTag loc)
                (void . move t)
                encLoc
        )
        (\r -> #previousRoom .= tagRoomEntity r) obj'
    pure obj


makeNameImproper :: (WithLabel "nameProperness" NameProperness x, State x :> es) => Eff es ()
makeNameImproper = #nameProperness .= Improper

makeNameProper :: (WithLabel "nameProperness" NameProperness x, State x :> es) => Eff es ()
makeNameProper = #nameProperness .= Proper