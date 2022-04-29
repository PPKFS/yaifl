{-|
Module      : Yaifl.Objects.Missing
Description : Constraints and handling for blocks where missing object lookups are failures, rather than
needing to fuss with `Maybe` everywhere.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Objects.Missing 
  ( -- * Types
    MissingObject(..)
  , NoMissingObjects
  , withoutMissingObjects
  , handleMissingObject
  , failHorriblyIfMissing
    -- * Lenses
  , moExpected
  , moEntity
  ) where

import Solitude
import Yaifl.Common (Entity)
import Control.Monad.Except (MonadError)
import Yaifl.Logger
import qualified Data.Text.Lazy.Builder as TLB

-- | A missing object is a textual representation of what the object was intended to be and the entity that was queried.
data MissingObject = MissingObject 
  { _moExpected :: Text
  , _moEntity :: Entity
  } deriving stock (Eq, Show, Read, Ord, Generic)

-- | A block with no missing objects is just a `MonadError`.
type NoMissingObjects m = MonadError MissingObject m

makeLenses ''MissingObject

-- | Run a block of code with a handler for missing objects
-- TODO: add logging here
withoutMissingObjects :: 
  forall m a. 
  HasCallStack
  => Monad m
  => (HasCallStack => ExceptT MissingObject m a) -- ^ the block
  -> (HasCallStack => MissingObject -> m a)  -- ^ the handler
  -> m a
withoutMissingObjects f def = runExceptT f >>= either def return

-- | A default handler for missing objects that returns a default after logging an error
handleMissingObject :: 
  HasCallStack
  => Logger m 
  => TLB.Builder 
  -> m a 
  -> MissingObject
  -> m a
handleMissingObject msg def (MissingObject t o) = do
  err (msg <> bformat (stext %! "; Object ID: " %! stext) t (show o))
  def

failHorriblyIfMissing ::
  HasCallStack
  => Logger m 
  => (HasCallStack => ExceptT MissingObject m a)
  -> m a
failHorriblyIfMissing f = withoutMissingObjects f (\(MissingObject t o) -> do
  let msg = "Failing horribly and erroring out because we can't recover"
      emsg = msg <> bformat (stext %! "; Object ID: " %! stext) t (show o)
  err emsg
  error $ show emsg)