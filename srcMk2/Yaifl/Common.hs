-- ~\~ language=Haskell filename=src/Yaifl/Common.hs
-- ~\~ begin <<lit/other_miscellania.md|src/Yaifl/Common.hs>>[0]
{-# OPTIONS_GHC -Wno-orphans #-}

module Yaifl.Common
  (-- * Datatypes
  Entity(..)
  , Store(..)
  , HasID(..)
  , Timestamp(..)
  , WorldModel(..)
  , RoomDescriptions(..)
  , WorldStage(..)
  
  , defaultVoidID
  , emptyStore
  -- * Object querying
  , isThing
  , isRoom
    -- * Type family nonsense
  , WMObjSpecifics
  , WMValues
  , WMDirections

  , WMShow
  , WMRead
  , WMOrd
  , WMEq
  )
where

import Solitude
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import Display

instance {-# OVERLAPPABLE #-} Display a where
  display = const "No display instance"

-- ~\~ begin <<lit/foundations/entities.md|entity-def>>[0]
newtype Entity = Entity
  { unID :: Int
  } deriving stock   (Show, Generic)
    deriving newtype (Eq, Num, Read, Bounded, Hashable, Enum, Ord, Real, Integral)
-- ~\~ end
-- ~\~ begin <<lit/foundations/entities.md|thing-or-room>>[0]
isThing ::
  (HasID a)
  => a
  -> Bool
isThing a = getID a >= 0

isRoom ::
  (HasID a)
  => a
  -> Bool
isRoom = not . isThing
-- ~\~ end
-- ~\~ begin <<lit/foundations/entities.md|has-id>>[0]
class HasID n where
  getID :: n -> Entity

instance HasID Entity where
  getID = id
-- ~\~ end
-- ~\~ begin <<lit/foundations/entities.md|base-ids>>[0]
defaultVoidID :: Entity
defaultVoidID = Entity (-1)

defaultNothingID :: Entity
defaultNothingID = Entity 0

defaultPlayerID :: Entity
defaultPlayerID = Entity 1
-- ~\~ end
-- ~\~ begin <<lit/foundations/entities.md|store-def>>[0]
-- import qualified Data.EnumMap.Strict as EM
newtype Store a = Store
  { unStore :: EM.EnumMap Entity a
  } deriving stock   (Show, Generic)
    deriving newtype (Eq, Ord, Read)

emptyStore :: Store a
emptyStore = Store EM.empty
-- ~\~ end
-- ~\~ begin <<lit/foundations/entities.md|alter-store>>[0]
alterEMF :: 
  (Functor f, Enum k)
  => (Maybe a -> f (Maybe a))
  -> k
  -> EM.EnumMap k a 
  -> f (EM.EnumMap k a)
alterEMF upd k m = EM.intMapToEnumMap <$> IM.alterF upd (fromEnum k) (EM.enumMapToIntMap m)

alterNewtypeEMF :: 
  (Functor f, Enum k)
  => (Maybe a -> f (Maybe a))
  -> k
  -> (nt -> EM.EnumMap k a)
  -> (EM.EnumMap k a -> nt)
  -> nt
  -> f nt
alterNewtypeEMF upd k unwrap wrap' m = wrap' <$> alterEMF upd k (unwrap m)
-- ~\~ end
-- ~\~ begin <<lit/foundations/entities.md|store-at>>[0]
instance At (Store a) where
  at k = lensVL $ \f -> alterNewtypeEMF f k unStore Store
-- ~\~ end
-- ~\~ begin <<lit/foundations/entities.md|store-instances>>[0]
type instance IxValue (Store a) = a
type instance Index (Store a) = Entity
instance Ixed (Store a)
-- ~\~ end

-- ~\~ begin <<lit/foundations/worldmodel.md|world-model>>[0]
data WorldModel = WorldModel Type Type Type Type
-- ~\~ end
-- ~\~ begin <<lit/foundations/worldmodel.md|world-model-families>>[0]
type family WMObjSpecifics (wm :: WorldModel) :: Type where
  WMObjSpecifics ('WorldModel objSpec dir o v) = objSpec

type family WMDirections (wm :: WorldModel) :: Type where
  WMDirections ('WorldModel objSpec dir o v) = dir 

type family WMValues (wm :: WorldModel) :: Type where
  WMValues ('WorldModel objSpec dir o v) = o
-- ~\~ end
-- ~\~ begin <<lit/foundations/worldmodel.md|world-model-constraints>>[0]
type WMConstr (c :: Type -> Constraint) wm = (c (WMObjSpecifics wm), c (WMValues wm), c (WMDirections wm))
type WMShow wm = WMConstr Show wm
type WMRead wm = WMConstr Read wm
type WMOrd wm = WMConstr Ord wm
type WMEq wm = WMConstr Eq wm
-- ~\~ end
-- ~\~ end
