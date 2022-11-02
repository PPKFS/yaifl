module Effectful.Optics
  ( (.=)
  , (%=)
  , (?=)
  , use
  ) where

import Effectful.State.Static.Shared (modify, State, gets)
import Effectful
import Solitude

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