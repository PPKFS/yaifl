
module Yaifl.PolysemyOptics
(
  assign
, (%=)
, modifying
, (.=)
, zoom
, use
, (?=)
) where

import Yaifl.Prelude
import Polysemy.State
import Polysemy



infix 4 .=, %=

use :: (Is k A_Getter, Member (State s) r) => Optic' k is s a -> Sem r a
use l = view l <$> get

modifying :: (Is k A_Setter, Member (State s) r) => Optic k is s s a b -> (a -> b) -> Sem r ()
modifying l f = modify (l %~ f)

(%=) :: (Is k A_Setter, Member (State s) r) => Optic k is s s a b -> (a -> b) -> Sem r ()
(%=) = modifying
{-# INLINE (%=) #-}

assign :: (Is k A_Setter, Member (State s) r) => Optic k is s s a b -> b -> Sem r ()
assign l v = modify (l .~ v)

(.=) :: (Is k A_Setter, Member (State s) r) => Optic k is s s a b -> b -> Sem r ()
(.=) = assign
{-# INLINE (.=) #-}

(?=) :: (Is k A_Setter, Member (State s) r) => Optic k is s s a (Maybe b) -> b -> Sem r ()
l ?= v = assign l (Just v)

zoom :: (Is k A_Lens, Member (State s) c) => Optic' k is s t -> Sem (State t ': c) r -> Sem c r
zoom o = interpret \case
  Get -> use (castOptic @A_Lens o)
  Put a -> assign (castOptic @A_Lens o) a
