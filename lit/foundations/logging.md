# Logging

The logging is currently a thin wrapper around `katip`, which does some lovely and customisable logging.

```haskell file=src/Yaifl/Logger.hs

{-|
Module      : Yaifl.Logger
Description : A wrapper around `Katip.Logger` that means I can swap it out more easily if I want to.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Logger 
  ( -- * Logging functions
    Logger(..)
  , YaiflItem(..)
  , toLoc
  , jsonFormatYaifl
  ) where

import Solitude
import Katip
import Language.Haskell.TH ( Loc(..) )
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Aeson as A
import qualified Data.Text as T
import Katip.Format.Time (formatAsLogTime)
import Katip.Scribes.Handle (colorBySeverity)
import GHC.Stack.Types

-- | An abstract interface for logging functions which are capable of reporting source locations.
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

<<callstack-logging>>
<<log-item>>
<<log-json>>
```

```haskell id=callstack-logging
-- | Try to extract the last callsite from some GHC 'CallStack' and convert it
-- to a 'Loc' so that it can be logged with 'logItemM'.
toLoc :: 
  CallStack 
  -> Maybe Loc
toLoc stk = (listToMaybe . reverse $ getCallStack stk) <&> \(_, loc) -> 
  Loc
    { loc_filename = srcLocFile loc,
      loc_package = srcLocPackage loc,
      loc_module = srcLocModule loc,
      loc_start = (srcLocStartLine loc, srcLocStartCol loc),
      loc_end = (srcLocEndLine loc, srcLocEndCol loc)
    }
```

```haskell id=log-item
newtype YaiflItem a = YaiflItem
  { toKatipItem :: Item a
  } deriving stock (Show, Eq)
    deriving newtype (Generic, Functor)

instance A.ToJSON a => A.ToJSON (YaiflItem a) where
    toJSON (YaiflItem Item{..}) = A.object $
      [ "level" A..= _itemSeverity
      , "message" A..= B.toLazyText (unLogStr _itemMessage)
      , "timestamp" A..= formatAsLogTime _itemTime
      , "ns" A..= let f = T.intercalate "➤" (filter (/= T.empty) $ unNamespace _itemNamespace) in if T.empty == f then "" else "❬"<>f<>"❭"
      , "loc" A..= fmap reshapeFilename _itemLoc
      ] ++ ["data" A..=  _itemPayload | A.encode _itemPayload /= "{}"]
```

```haskell id=log-json
-- | Convert an absolute filename into...something else? I'm not sure.
reshapeFilename :: 
  Loc 
  -> String
reshapeFilename Loc{..} = drop 1 (dropWhile (/= '/') loc_filename) <> ":" <> show (fst loc_start) <> ":" <> show (snd loc_start)

-- | Convert log item to its JSON representation while trimming its
-- payload based on the desired verbosity. Backends that push JSON
-- messages should use this to obtain their payload.
itemJsonYaifl :: 
  LogItem a
  => Verbosity
  -> YaiflItem a
  -> A.Value
itemJsonYaifl verb (YaiflItem a) = A.toJSON
  $ YaiflItem $ a { _itemPayload = payloadObject verb (_itemPayload a) }

-- | A formatter for making Items.
jsonFormatYaifl :: 
  LogItem a 
  => ItemFormatter a
jsonFormatYaifl withColor verb i =
  B.fromText $
  colorBySeverity withColor (_itemSeverity i) $
  toStrict $ decodeUtf8 $ A.encode $ itemJsonYaifl verb (YaiflItem i)
```