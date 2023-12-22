{-|
Module      : Yaifl.Model.Objects.Store
Copyright   : (c) Avery 2023
License     : MIT
Maintainer  : ppkfs@outlook.com

A small wrapper around `EM.EnumMap`s specialised for `Entity`; mostly this needs extra
work to get `At` and `Ixed` instances working through the newtype wrappers.
-}

module Yaifl.Model.Objects.Store
  ( -- * Stores
    Store(..)
  , emptyStore
  ) where

import Solitude
import Yaifl.Model.Objects.Entity
import qualified Data.EnumMap as EM
import qualified Data.IntMap as IM

-- | An `EnumMap` specialised over `Entity`s.
newtype Store a = Store
  { unStore :: EM.EnumMap Entity a
  } deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Read, Foldable)

-- | A store with no items.
emptyStore :: Store a
emptyStore = Store EM.empty

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

instance At (Store a) where
  at k = lensVL $ \f -> alterNewtypeEMF f k unStore Store

type instance IxValue (Store a) = a
type instance Index (Store a) = Entity
instance Ixed (Store a)