# Logging

The logging is currently a thin wrapper around `katip`, which does some lovely and customisable logging. Mostly we use an effect here to 

```haskell file=src/Yaifl/Logger.hs
{-# LANGUAGE RecordWildCards #-}

module Yaifl.Logger 
  ( -- * Logging functions
    Logger(..)
  , YaiflItem(..)
  , toLoc
  , jsonFormatYaifl
  ) where

import Solitude
import Language.Haskell.TH ( Loc(..) )
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Aeson as A
import qualified Data.Text as T
import GHC.Stack.Types

data Log m where
  LogMsg :: MsgSeverity -> Log m () 
  AddContext :: Text -> Log m ()
  PopContext :: Log m ()
  
  debug :: HasCallStack => TLB.Builder -> m ()
  info :: HasCallStack => TLB.Builder -> m ()
  warn :: HasCallStack => TLB.Builder -> m ()
  err :: HasCallStack => TLB.Builder -> m ()
  withContext :: HasCallStack => TLB.Builder -> m a -> m a

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
    deriving newtype (Functor)

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
