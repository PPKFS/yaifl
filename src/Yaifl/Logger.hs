module Yaifl.Logger where

import Solitude
import Katip
import Language.Haskell.TH ( Loc(..) )
import qualified Data.Text.Lazy.Builder as TLB
import GHC.Stack

-- | An abstract interface for logging functions which are capable of reporting
-- source locations.
class Monad m => Logger m where
  debug :: HasCallStack => TLB.Builder -> m ()
  info :: HasCallStack => TLB.Builder -> m ()
  warn :: HasCallStack => TLB.Builder -> m ()
  err :: HasCallStack => TLB.Builder -> m ()
  withContext :: HasCallStack => TLB.Builder -> m a -> m a

instance Logger m => Logger (ExceptT e m) where
  debug = lift . debug
  info = lift . info
  warn = lift . warn
  err = lift . err
  withContext b (ExceptT f) = ExceptT (withContext b f)

instance Logger m => Logger (MaybeT m) where
  debug = lift . debug
  info = lift . info
  warn = lift . warn
  err = lift . err
  withContext b (MaybeT f) = MaybeT (withContext b f)

-- | Try to extract the last callsite from some GHC 'CallStack' and convert it
-- to a 'Loc' so that it can be logged with 'logItemM'.
toLoc :: CallStack -> Maybe Loc
toLoc stk =
  let mLoc = listToMaybe . reverse $ getCallStack stk
   in mLoc <&> \(_, loc) ->
        Loc
          { loc_filename = srcLocFile loc,
            loc_package = srcLocPackage loc,
            loc_module = srcLocModule loc,
            loc_start = (srcLocStartLine loc, srcLocStartCol loc),
            loc_end = (srcLocEndLine loc, srcLocEndCol loc)
          }