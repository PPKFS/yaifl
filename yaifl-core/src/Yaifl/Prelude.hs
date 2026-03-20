{-|
Module      : Yaifl.Prelude
Copyright   : (c) Avery 2024-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Custom prelude that wraps @relude@ and adds some additional useful functions I've picked up.
Also includes stateful operators for compatibility between @optics@ and @effectful@.
-}

module Yaifl.Prelude
  ( -- * Custom Types
    Pointed(..)

  -- * Re-exports
  , module Named
  , module Data.Text.Display
  , module Relude
  , module Optics
  , module Relude.Extra.Bifunctor
  , module Relude.Extra.Tuple
  , module Effectful
  , module Effectful.State.Dynamic

  -- * Monadic Utilities
  , whileM
  , caseM
  , surroundM

  -- * Collection Processing
  , composel
  , bothAnd
  , eitherJoin
  , thenATraverse

  -- * Text Utilities
  , isPrefixOf'
  , wrap
  , prettyPrintList

  -- * Optics Utilities
  , universeSans
  , (<<+~)
  , (<<-~)
  , (<$?>)
  , (.=)
  , (?=)
  , (%=)
  , (%%=)
  , use
  , (<<%=)
  , reversed

  -- * Stateful Operations
  , WithLabel
  , runLocalState
  ) where

import Relude hiding (Down, State, get, put, modify, gets, state, modify', runState, evalState, execState)
import Optics hiding
  (uncons, zoom, gviews, zoomMaybe, use, gview, preuse, modifying', modifying, assign', assign)
import Data.List ((\\))
import Data.Text.Display hiding ( Opaque )
import Effectful
import Effectful.State.Dynamic
import Named hiding ( Name )
import Optics.State.Operators ( PermeableOptic(..) )
import Relude.Extra.Bifunctor
import Relude.Extra.Tuple
import qualified Effectful.State.Dynamic as State
import qualified Data.List.NonEmpty as NonEmpty

-- | Obtain a list of all members of a type universe, sans a finite list
universeSans
  :: Bounded x
  => Enum x
  => Ord x
  => [x]
  -> [x]
universeSans x = universe \\ x

class Reversing t where
  reversing :: t -> t

instance Reversing (NonEmpty a) where
  reversing = NonEmpty.reverse

reversed :: Reversing a => Iso' a a
reversed = involuted reversing

instance Reversing [a] where
  reversing = reverse

bothAnd ::
  a
  -> a
  -> (a -> Bool)
  -> Bool
bothAnd a1 a2 f = f a1 && f a2

-- | generalised version of `isPrefixOf` for when the lists are of different types
isPrefixOf' ::
  (a -> b -> Bool)
  -> [a]
  -> [b]
  -> Bool
isPrefixOf' _ [] _ = True
isPrefixOf' _ (_:_) [] = False
isPrefixOf' eq (l:ls) (x:xs) = eq l x && isPrefixOf' eq ls xs

-- | `case` equivalent over a list of monadic `Maybe`s.
caseM
  :: Monad m
  => [MaybeT m a]
  -> m a
  -> m a
caseM cases fallback = runMaybeT (asum cases) >>= maybe fallback pure

-- | surround a semigroup with a start and an end.
wrap
  :: Semigroup a
  => a
  -> a
  -> a
wrap a b = a <> b <> a

-- | fold a list of endomorphisms.
composel
  :: Foldable f
  => f (a -> a)
  -> a
  -> a
composel = foldl' (.) id

-- | a version of `bracket` that just keeps the surrounding behaviour and not the finality behaviour
-- i.e. it doesn't need IO.
surroundM
  :: Monad m
  => m a -- ^ how to set it up
  -> m b -- ^ what to do
  -> (a -> m c) -- ^ how to take it apart again
  -> m b
surroundM pre' doIt post = do
  p' <- pre'
  r <- doIt
  _ <- post p'
  return r

-- | Double up a functor.
(<<$>>) ::
  Functor f
  => Functor g
  => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

infixl 4 <$?>

-- | `fmap` to `Bool` where a `Nothing` is considered `False`.
(<$?>)
  :: (a -> Bool)
  -> Maybe a
  -> Bool
f <$?> m = maybe False f m

-- | Format a list of items with proper English conjunctions.
--
-- Converts a list of text items into a properly formatted English list
-- with appropriate use of commas and "and" for the final conjunction.
--
-- This is particularly useful for generating natural-sounding descriptions
-- such as listing objects or inventory items.
--
-- Examples:
-- @
--   prettyPrintList []          == ""
--   prettyPrintList ["apple"]   == "apple"
--   prettyPrintList ["apple", "orange"] == "apple, and orange"
--   prettyPrintList ["apple", "orange", "pear"] == "apple, orange, and pear"
-- @
prettyPrintList :: [Text] -> Text
prettyPrintList [] = ""
prettyPrintList [x] = x
prettyPrintList [x, y] = x <> ", and " <> y
prettyPrintList (x:xs) = x <> ", " <> prettyPrintList xs

infixr 4 <<+~, <<-~

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

-- | Pointed set class; Monoid without the operation, or the dreaded default typeclass.
class Pointed s where
  identityElement :: s
{-}
instance {-# OVERLAPPABLE #-} Monoid m => Pointed m where
  identityElement = mempty
-}
instance Pointed () where
  identityElement = ()

-- | Monadic while loop.
--
-- Executes a monadic action repeatedly while the predicate holds true.
-- This is useful for implementing game loops, input processing, or
-- any operation that needs to repeat until a condition is met.
whileM :: Monad m => (a -> Bool)  -- ^ Continuation predicate
                -> m a             -- ^ Action to repeat
                -> m a             -- ^ Final result
whileM pr f = do
  a <- f
  if pr a then whileM pr f else return a


type And :: ([k] -> Constraint) -> ([k] -> Constraint) -> [k] -> Constraint

class And c1 c2 l
instance (c1 l, c2 l) => And c1 c2 l


-- | Stateful assignment operator.
--
-- Sets the value targeted by an optic in the state monad.
-- This is the effectful equivalent of (`%=` `const`).
(.=)
  :: Is k A_Setter
  => State s :> es
  => Optic k is s s a b
  -> b
  -> Eff es ()
(.=) o = modify . over o . const

-- | Stateful assignment to a `Maybe` field.
--
-- Sets a `Maybe`-typed field to `Just` the given value.
-- Useful for optional fields where you want to ensure a value is present.
(?=)
  :: Is k A_Setter
  => State s :> es
  => Optic k is s s (Maybe a) (Maybe b)
  -> b
  -> Eff es ()
(?=) o = (modify . over o) . const . Just

-- | Stateful modification operator.
--
-- Modifies the value targeted by an optic using a transformation function.
-- This is the effectful equivalent of the optics (`%~`) operator.
(%=)
  :: Is k A_Setter
  => State s :> es
  => Optic k is s s a b
  -> (a -> b)
  -> Eff es ()
(%=) = (modify .) . over

-- | Get the value targeted by an optic.
--
-- Retrieves the current value of a field using an optic.
-- This is the effectful equivalent of the optics (`^.`) operator.
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

type WithLabel sym ty o = LabelOptic' sym A_Lens o ty

-- | Run a stateful computation with local state.
--
-- Executes an effectful computation that uses a local `State` effect,
-- returning the final state value. This is useful for isolated stateful
-- operations that don't need to be part of a larger effect stack.
--
-- Note: This function discards the computation result and only returns
-- the final state. Use `runState` if you need both the result and state.
runLocalState :: a1 -> Eff '[State a1] a2 -> a1
runLocalState bl upd = snd $ runPureEff $ runStateLocal bl upd