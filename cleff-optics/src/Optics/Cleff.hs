
module Optics.Cleff where

import Cleff
import Cleff.State
import Cleff.Reader
import Cleff.Writer
import Optics.Core
import Data.Kind

modify' :: State s :> es => (s -> s) -> Eff es ()
modify' f = get >>= (($!) put . f)

-- we need this for `PermeableOptic`, even though we can't use `guse`/`guses` because we lack the simple MonadReader
-- stuff.
class ViewableOptic k r where
  type ViewResult k r :: Type
  gview
    :: Reader s :> es
    => Optic' k is s r
    -> Eff es (ViewResult k r)
  gviews
    :: Reader s :> es
    => Optic' k is s a
    -> (a -> r)
    -> Eff es (ViewResult k r)

instance ViewableOptic An_Iso r where
  type ViewResult An_Iso r = r
  gview    = asks . view
  gviews o = asks . views o
  {-# INLINE gview #-}
  {-# INLINE gviews #-}

instance ViewableOptic A_Lens r where
  type ViewResult A_Lens r = r
  gview    = asks . view
  gviews o = asks . views o
  {-# INLINE gview #-}
  {-# INLINE gviews #-}

instance ViewableOptic A_ReversedPrism r where
  type ViewResult A_ReversedPrism r = r
  gview    = asks . view
  gviews o = asks . views o
  {-# INLINE gview #-}
  {-# INLINE gviews #-}

instance ViewableOptic A_Getter r where
  type ViewResult A_Getter r = r
  gview    = asks . view
  gviews o = asks . views o
  {-# INLINE gview #-}
  {-# INLINE gviews #-}

instance ViewableOptic A_Prism r where
  type ViewResult A_Prism r = Maybe r
  gview    = asks . preview
  gviews o = asks . previews o
  {-# INLINE gview #-}
  {-# INLINE gviews #-}

instance ViewableOptic An_AffineTraversal r where
  type ViewResult An_AffineTraversal r = Maybe r
  gview    = asks . preview
  gviews o = asks . previews o
  {-# INLINE gview #-}
  {-# INLINE gviews #-}

instance ViewableOptic An_AffineFold r where
  type ViewResult An_AffineFold r = Maybe r
  gview    = asks . preview
  gviews o = asks . previews o
  {-# INLINE gview #-}
  {-# INLINE gviews #-}

instance Monoid r => ViewableOptic A_Traversal r where
  type ViewResult A_Traversal r = r
  gview    = asks . foldOf
  gviews o = asks . foldMapOf o
  {-# INLINE gview #-}
  {-# INLINE gviews #-}

instance Monoid r => ViewableOptic A_Fold r where
  type ViewResult A_Fold r = r
  gview    = asks . foldOf
  gviews o = asks . foldMapOf o
  {-# INLINE gview #-}
  {-# INLINE gviews #-}

class (Is k A_Traversal, ViewableOptic k r) => PermeableOptic k r where
  -- | Modify the target of an 'Optic' returning extra information of type 'r'.
  passthrough
    :: Optic k is s t a b
    -> (a -> (r, b))
    -> s
    -> (ViewResult k r, t)

instance PermeableOptic An_Iso r where
  passthrough o = toLensVL o
  {-# INLINE passthrough #-}

instance PermeableOptic A_Lens r where
  passthrough o = toLensVL o
  {-# INLINE passthrough #-}

instance PermeableOptic A_Prism r where
  passthrough o f s = withPrism o $ \bt sta -> case sta s of
    Left t -> (Nothing, t)
    Right a -> case f a of
      (r, b) -> (Just r, bt b)
  {-# INLINE passthrough #-}

instance PermeableOptic An_AffineTraversal r where
  passthrough o f s = withAffineTraversal o $ \sta sbt -> case sta s of
    Left t -> (Nothing, t)
    Right a -> case f a of
      (r, b) -> (Just r, sbt s b)
  {-# INLINE passthrough #-}

instance Monoid r => PermeableOptic A_Traversal r where
  passthrough = traverseOf
  {-# INLINE passthrough #-}

modifying :: (Is k A_Setter, State s :> es) => Optic k is s s a b -> (a -> b) -> Eff es ()
modifying o = modify . over o
{-# INLINE modifying #-}

modifying' :: (Is k A_Setter, State s :> es) => Optic k is s s a b -> (a -> b) -> Eff es ()
modifying' o = modify' . over' o
{-# INLINE modifying' #-}

assign :: (Is k A_Setter, State s :> es) => Optic k is s s a b -> b -> Eff es ()
assign o = modifying o . const
{-# INLINE assign #-}

assign' :: (Is k A_Setter, State s :> es) => Optic k is s s a b -> b -> Eff es ()
assign' o = modifying' o . const
{-# INLINE assign' #-}

use :: (Is k A_Getter, State s :> es) => Optic' k is s a -> Eff es a
use o = gets (view o)
{-# INLINE use #-}

preuse :: (Is k An_AffineFold, State s :> es) => Optic' k is s a -> Eff es (Maybe a)
preuse o = gets (preview o)
{-# INLINE preuse #-}

infix 4 .=
(.=) :: (Is k A_Setter, State s :> es) => Optic k is s s a b -> b -> Eff es ()
(.=) = assign
{-# INLINE (.=) #-}

infix 4 ?=
(?=) :: (Is k A_Setter, State s :> es) => Optic k is s s (Maybe a) (Maybe b) -> b -> Eff es ()
(?=) = \o -> assign o . Just
{-# INLINE (?=) #-}

infix 4 %=
(%=) :: (Is k A_Setter, State s :> es) => Optic k is s s a b -> (a -> b) -> Eff es ()
(%=) = modifying
{-# INLINE (%=) #-}

infix 4 %%=
(%%=) :: (PermeableOptic k r, State s :> es) => Optic k is s s a b -> (a -> (r, b)) -> Eff es (ViewResult k r)
o %%= f = state (passthrough o f)
{-# INLINE (%%=) #-}

infix 4 <.=
(<.=) :: (PermeableOptic k b, State s :> es) => Optic k is s s a b -> b -> Eff es (ViewResult k b)
o <.= b = o <%= const b
{-# INLINE (<.=) #-}

infix 4 <?=
(<?=) :: (PermeableOptic k (Maybe b), State s :> es) => Optic k is s s (Maybe a) (Maybe b) -> b -> Eff es (ViewResult k (Maybe b))
o <?= b = o <.= Just b
{-# INLINE (<?=) #-}

infix 4 <%=
(<%=) :: (PermeableOptic k b, State s :> es) => Optic k is s s a b -> (a -> b) -> Eff es (ViewResult k b)
o <%= f = o %%= \a -> let b = f a in (b, b)
{-# INLINE (<%=) #-}

infix 4 <<.=
(<<.=) :: (PermeableOptic k a, State s :> es) => Optic k is s s a b -> b -> Eff es (ViewResult k a)
o <<.= b = o <<%= const b
{-# INLINE (<<.=) #-}

infix 4 <<?=
(<<?=) :: (PermeableOptic k (Maybe a), State s :> es) => Optic k is s s (Maybe a) (Maybe b) -> b -> Eff es (ViewResult k (Maybe a))
o <<?= b = o <<.= Just b
{-# INLINE (<<?=) #-}

infix 4 <<%=
(<<%=) :: (PermeableOptic k a, State s :> es) => Optic k is s s a b -> (a -> b) -> Eff es (ViewResult k a)
o <<%= f = o %%= \a -> (a, f a)
{-# INLINE (<<%=) #-}
{-
guse :: (ViewableOptic k a, State s :> es) => Optic' k is s a -> Eff es (ViewResult k a)
guse o = absorbState (Optics.guse o)
{-# INLINE guse #-}

guses :: (ViewableOptic k r, State s :> es) => Optic' k is s a -> (a -> r) -> Eff es (ViewResult k r)
guses o f = absorbState (Optics.guses o f)
{-# INLINE guses #-}
-}
zoom :: (Is k A_Lens, State s :> es) => Optic' k is s a -> Eff (State a ': es) c -> Eff es c
zoom o = interpret \case
  Get     -> gets (^. o')
  Put s   -> modify (& o' .~ s)
  State f -> state \t -> let (a, !s) = f (t ^. o') in (a, t & o' .~ s)
  where o' = castOptic @A_Lens o
{-# INLINE zoom #-}

zoomMaybe :: (Is k An_AffineTraversal, State s :> es) => Optic' k is s a -> Eff (State a ': es) c -> Eff es (Maybe c)
zoomMaybe o m = preuse o' >>= traverse \a ->
  ( interpret \case
      Get    -> maybe a id <$> preuse o'
      Put a' -> assign o' a'
      State f -> error "I have no idea what to put here"
  ) m
  where o' = castOptic @An_AffineTraversal o
{-# INLINE zoomMaybe #-}

