module Yaifl.Backdrop.Create
  ( addBackdrop
  ) where

import Yaifl.Prelude

import Yaifl.Entity
import Yaifl.Object.Kind
import Yaifl.Object.Create
import Yaifl.Thing.Kind
import Yaifl.Enclosing.Kind ( Enclosing (..) )
import Yaifl.Property.Has ( WMWithProperty )
import Yaifl.MultiLocated.Kind
import qualified Data.Set as S
import Yaifl.Backdrop.Kind
import Yaifl.WorldModel
import Yaifl.Effects.RuleEffects
import Yaifl.ObjectSpecifics
import Yaifl.Thing.Create
import Yaifl.MultiLocated.Query

addBackdrop ::
  forall wm es.
  WMHasObjSpecifics wm
  => WMWithProperty wm Enclosing
  => WMWithProperty wm MultiLocated
  => RuleEffects wm es
  => AddObjects wm es
  => WMText wm -- ^ name
  -> "initialAppearance" :? WMText wm
  -> "description" :? WMText wm -- ^ Description.
  -> "described" :? ThingDescribed
  -> "modify" :? Eff '[State (Thing wm)] () -- ^ Build your own thing monad!
  -> "locations" :! NonEmpty EnclosingEntity
  -> Eff es ThingEntity
addBackdrop n ia des (argDef #described Described -> desc) (argDef #modify pass -> upD) (argF #locations -> Identity (l:|ls)) = do
  d <- addThing @wm n ia des
    ! #specifics (inj (Proxy @wm) $ BackdropSpecifics (Backdrop (MultiLocated (S.fromList $ l:ls))))
    ! #modify (do
      upD
      -- A backdrop is usually scenery.
      makeItScenery
      #objectData % #portable .= FixedInPlace
      #objectData % #pushableBetweenRooms .= False
      #objectData % #described .= desc)
    ! #type (ObjectKind "backdrop")
    ! #location l
    ! done
  updateMultiLocatedObject d
  pure d