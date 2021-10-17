module Yaifl.Prelude
  (
    module Relude
  , module Optics
  , module Optics.State.Operators
  , alterNewtypeEMF
  , reversed
  , mapMaybeM
  , maybeOrReport2
  , (<<+~)
  , (<<-~)
  , (<$$>)
  , eitherJoin
  , thenATraverse
  , universeSans
  ) where

import Relude hiding (mapMaybeM)
import Optics hiding (uncons)
import Optics.State.Operators

import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import qualified Data.List.NonEmpty as NonEmpty
import Data.List ((\\))
import qualified Data.EnumSet
-- first let's define our own alterF for EnumMap...
alterEMF
  :: (Functor f, Enum k)
  => (Maybe a -> f (Maybe a))
  -> k
  -> EM.EnumMap k a -> f (EM.EnumMap k a)
alterEMF upd k m = EM.intMapToEnumMap <$> IM.alterF upd (fromEnum k) (EM.enumMapToIntMap m)

-- | alterF wrapper for Store, since it's a wrapper around a wrapper...
alterNewtypeEMF
  :: Functor f
  => Enum k
  => (Maybe a -> f (Maybe a))
  -> k
  -> (nt -> EM.EnumMap k a)
  -> (EM.EnumMap k a -> nt)
  -> nt
  -> f nt
alterNewtypeEMF upd k unwrap wrap' m = wrap' <$> alterEMF upd k (unwrap m)

class Reversing t where
  reversing :: t -> t

instance Reversing (NonEmpty a) where
  reversing = NonEmpty.reverse

reversed :: Reversing a => Iso' a a
reversed = involuted reversing

instance Reversing [a] where
  reversing = reverse

infixr 4 <<+~, <<-~

mapMaybeM
  :: (Monad m)
  => Maybe a
  -> (a -> m b)
  -> m (Maybe b)
mapMaybeM m f = maybe (return Nothing) (fmap Just . f) m

-- | Increment the target of a 'PermeableOptic' into your 'Monad''s state by a
-- number function and return the /old/ value that was replaced.
(<<+~)
  :: Num a
  => Optic A_Lens is s s a a
  -> a
  -> s
  -> (a, s)
(<<+~) l b s = (s ^. l, s & l %~ (+b))
{-# INLINE (<<+~) #-}

-- | Decrement the target of a 'PermeableOptic' into your 'Monad''s state by a
-- number function and return the /old/ value that was replaced.
(<<-~)
  :: Num a
  => Optic A_Lens is s s a a
  -> a
  -> s
  -> (a, s)
(<<-~) l b s = (s ^. l, s & l %~ (\x -> x - b))
{-# INLINE (<<-~) #-}

maybeOrReport2
  :: Monad m
  => Maybe a
  -> Maybe b
  -> m ()
  -> m ()
  -> (a -> b -> m c)
  -> m (Maybe c)
maybeOrReport2 c1 c2 err1 err2 f = do
    when (isNothing c1) err1
    when (isNothing c2) err2
    sequenceA (f <$> c1 <*> c2)

infixl 4 <$$>
(<$$>)
  :: Functor f
  => Functor g
  => (a -> b)
  -> f (g a)
  -> f (g b)
h <$$> m = fmap h <$> m

eitherJoin
  :: AffineTraversal' a f
  -> AffineTraversal' b f
  -> AffineTraversal' (Either a b) f
eitherJoin t1 t2 = (_Left % t1) `thenATraverse` (_Right % t2)

thenATraverse
  :: Is t1 An_AffineTraversal
  => Is t2 An_AffineTraversal
  => Optic t1 ix s s a b
  -> Optic t2 ix s s a b
  -> AffineTraversal s s a b
thenATraverse o1 o2 = atraversal
  ( \s -> case matching o1 s of
      Left _ -> matching o2 s
      Right f -> Right f
  )
  (\s b -> s & castOptic @An_AffineTraversal o1 .~ b
           & castOptic @An_AffineTraversal o2 .~ b
  )

  -- | Obtain a list of all members of a type universe, sans a finite list
universeSans
  :: Bounded x
  => Enum x
  => Ord x
  => [x]
  -> [x]
universeSans x = universe \\ x
