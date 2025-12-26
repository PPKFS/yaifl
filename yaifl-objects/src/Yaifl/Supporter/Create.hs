module Yaifl.Supporter.Create
  ( addSupporter

  ) where

import Yaifl.Prelude

import Yaifl.Entity
import Yaifl.Object.Kind
import Yaifl.Object.Create
import Yaifl.Thing.Kind
import Yaifl.Supporter.Kind
import Yaifl.Enclosing.Kind ( Enclosing (..), blankEnclosing )
import Yaifl.Property.Has ( WMWithProperty )
import Yaifl.Tag
import Yaifl.WorldModel
import Yaifl.ObjectSpecifics
import Yaifl.Thing.Create
import Yaifl.Container.Kind

addSupporter ::
  forall wm es.
  WMHasObjSpecifics wm
  => WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> "initialAppearance" :? WMText wm
  -> "description" :? WMText wm
  -> "carryingCapacity" :? Int
  -> "location" :? EnclosingEntity
  -> "enterable" :? Enterable
  -> "modify" :? Eff '[State (Thing wm)] ()
  -> Eff es SupporterEntity
addSupporter n ia d
  (argF #carryingCapacity -> cc) (argF #location -> l) (argF #enterable -> e) (argF #modify -> m) = do
    let enc = (blankEnclosing { capacity = cc <|> Just 100 })
        sup = Supporter enc (fromMaybe NotEnterable e)
    c <- addThing @wm n ia d
        ! #specifics (inj (Proxy @wm) $ SupporterSpecifics sup)
        ! #type (ObjectKind "supporter")
        ! paramF #location l
        ! paramF #modify m
        ! done
    pure $ tagEntity @_ @SupporterTag sup c
