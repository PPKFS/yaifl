{-|
Module      : Yaifl.Model.Objects.Store
Copyright   : (c) Avery 2023
License     : MIT
Maintainer  : ppkfs@outlook.com

A small wrapper around `EM.EnumMap`s specialised for `Entity`; mostly this needs extra
work to get `At` and `Ixed` instances working through the newtype wrappers.
-}

module Yaifl.Model.Store
  ( -- * Stores
    Store(..)
  , emptyStore
  ) where

import Yaifl.Prelude
import Yaifl.Model.Entity
import qualified Data.EnumMap as EM
import qualified Data.IntMap as IM

-- | An `EM.EnumMap` specialised over `Entity`s.
newtype Store a = Store
  { unStore :: EM.EnumMap Entity a
  } deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Read, Foldable, Functor)

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

instance Traversable Store where
  sequenceA (Store s) = Store <$> sequenceA s

type instance IxValue (Store a) = a
type instance Index (Store a) = Entity
instance Ixed (Store a)