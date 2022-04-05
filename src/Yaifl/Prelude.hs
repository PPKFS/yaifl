module Yaifl.Prelude
  (
  reversed
  , mapMaybeM
  , maybeOrReport2

  , (<$$>)
  , eitherJoin
  , thenATraverse
  , (<$?>)
  ) where

import Solitude hiding (mapMaybeM)

import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import qualified Data.List.NonEmpty as NonEmpty
import Data.List ((\\))


-- default (Integer, Double, Text)








mapMaybeM
  :: (Monad m)
  => Maybe a
  -> (a -> m b)
  -> m (Maybe b)
mapMaybeM m f = maybe (return Nothing) (fmap Just . f) m

infixl 4 <$?>

(<$?>)
  :: (a -> Bool)
  -> Maybe a
  -> Bool
f <$?> m = maybe False f m

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


