module Effectful.Optics
  ( (.=)
  , (%=)
  , (?=)
  , use
  , (%%=)
  , (<<%=)
  ) where

import Effectful.State.Dynamic (modify, State, gets)
import Effectful
import Solitude
import qualified Effectful.State.Dynamic as State
import Optics.State.Operators ( PermeableOptic(..) )

(.=)
  :: Is k A_Setter
  => State s :> es
  => Optic k is s s a b
  -> b
  -> Eff es ()
(.=) o = modify . over o . const

(?=)
  :: Is k A_Setter
  => State s :> es
  => Optic k is s s (Maybe a) (Maybe b)
  -> b
  -> Eff es ()
(?=) o = (modify . over o) . const . Just

(%=)
  :: Is k A_Setter
  => State s :> es
  => Optic k is s s a b
  -> (a -> b)
  -> Eff es ()
(%=) = (modify .) . over

use ::
  forall s a es is k.
  (Is k A_Getter, State s :> es)
  => Optic' k is s a
  -> Eff es a
use o = gets (view o)

infix 4 %%=
(%%=)
  :: (PermeableOptic k r, State s :> es)
  => Optic k is s s a b
  -> (a -> (r, b))
  -> Eff es (ViewResult k r)
o %%= f = State.state (passthrough o f)
{-# INLINE (%%=) #-}

(<<%=)
  :: (PermeableOptic k a, State s :> es)
  => Optic k is s s a b
  -> (a -> b)
  -> Eff es (ViewResult k a)
o <<%= f = o %%= toSnd f
{-# INLINE (<<%=) #-}