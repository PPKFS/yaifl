# Logging

With the move from `mtl` to `cleff`, it was probably less effort to just write my own logging effect than to find how to get e.g. [`co-log`](https://hackage.haskell.org/package/co-log) working with an arbitrary effects system. Plus, I don't need that much of the fancy stuff it does? I just want a context and to write to many different places.

```haskell file=src/Yaifl/Logger.hs
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Logger where

import Solitude hiding ( trace, local, asks, Reader, runReader )
import Language.Haskell.TH ( Loc(..) )
import qualified Data.Text.Lazy.Builder as TLB
import Cleff.Trace ( trace, Trace )
import Cleff.Reader ( Reader, asks, local )
import Data.Text.Display ( display, Display, ShowInstance(..) )
import GHC.Stack
import Data.Time ( getCurrentTime, UTCTime )
import qualified Data.Aeson as A
import qualified Data.Text as T
import Katip.Format.Time ( formatAsLogTime )

<<log-effect>>
```

## Logging Effect

We only need two functions here, one to log a message of any severity (optionally with a callstack, stolen from `katip`) and another to make a contexted block (so we can have `[Some][Context][Here]` at the start of our lines in a hierarchal way). 
```haskell id=log-effect
data MsgSeverity = Debug | Info | Warning | Error
  deriving stock (Eq, Ord, Enum, Bounded, Show)
  deriving (Display) via (ShowInstance MsgSeverity)

data Log :: Effect where
  LogMsg :: Maybe Loc -> MsgSeverity -> Text -> Log m () 
  WithContext :: Text -> m a -> Log m ()

makeEffect ''Log
<<log-interpreters>>
<<log-functions>>
<<callstack-log>>
<<log-item>>
```

### Interpreters

There already exist 3 different output-like effects in `cleff`; `Output` (for arbitrary `o`), `Trace` (which says it's for debugging logs), and `Writer` (which is even more general still). For the most flexibility, it's best to `reinterpret` our logging as a `Trace` effect (which has built-in interpreters to produce `Writer`s or `Output`s) with a `Reader` for the logging context. Of course, we may not want this `Reader` in the case we won't use it for anything (ignoring output) so we write our own ignore interpreter.

```haskell id=log-interpreters
runAndIgnoreLogging :: 
  Eff (Log : es) 
  ~> Eff es
runAndIgnoreLogging = interpret \case
  LogMsg _ _ _ -> pass
  WithContext _ m -> toEff $ void m

runLoggingAsTrace :: 
  [Reader [Text], IOE] :>> es 
  => Eff (Log : es) 
  ~> Eff (Trace : es)
runLoggingAsTrace = reinterpret \case
  LogMsg mbLoc sev msg -> do
    now <- liftIO getCurrentTime
    cxt <- asks reverse
    trace $ makeJSONObject now cxt sev mbLoc msg
  WithContext cxt m -> void $ local (cxt:) (toEff m)

makeJSONObject :: UTCTime -> [Text] -> MsgSeverity -> Maybe Loc -> Text -> String
makeJSONObject now cxt sev mbLoc pl = (decodeUtf8 $ A.encode $
  YaiflItem
  { itemSeverity = display sev
  , itemLoc = mbLoc
  , itemMessage = pl
  , itemTime = now
  , itemContext = cxt
  })
```

## Extracting callsite info

We try to extract the last callsite from some GHC `CallStack` and convert it to a `Loc`.

```haskell id=callstack-log
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

## Logging Functions

Each of our logging functions is a more concise way to log with a set severity and a callstack. This is inherently a structured logging format (perhaps in the future it'd be better to have the option of plaintext logging rather than explicitly and only JSON formatting) but JSON is what works nicely with `lnav`.

```haskell id=log-functions
logInternal ::
  HasCallStack
  => Log :> es
  => MsgSeverity
  -> TLB.Builder
  -> Eff es ()
logInternal sev msg = logMsg (toLoc callStack) sev (toText $ TLB.toLazyText msg)

debug :: 
  HasCallStack
  => Log :> es
  => TLB.Builder
  -> Eff es ()
debug = logInternal Debug

info :: 
  HasCallStack 
  => Log :> es
  => TLB.Builder
  -> Eff es ()
info = logInternal Info

warn :: 
  HasCallStack 
  => Log :> es
  => TLB.Builder
  -> Eff es ()
warn = logInternal Warning

err :: 
  HasCallStack 
  => Log :> es
  => TLB.Builder
  -> Eff es ()
err = logInternal Error
```

## Structured logging, at last

So we have everything done except the middle piece of glue that turns a logging message (probably with a callstack) into a JSON object. We need to relativise the filename to just `Foo.hs` and build the JSON object.

We use `katip` just because it has really nice time formatting that is nontrivial to repeat.

```haskell id=log-item

reshapeFilename :: 
  Loc 
  -> String
reshapeFilename Loc{..} = drop 1 (dropWhile (/= '/') loc_filename) <> ":" <> show (fst loc_start) <> ":" <> show (snd loc_start)

data YaiflItem = YaiflItem
  { itemSeverity :: Text
  , itemMessage :: Text
  , itemTime :: UTCTime
  , itemContext :: [Text]
  , itemLoc :: Maybe Loc
  } deriving stock (Show, Eq)

instance A.ToJSON YaiflItem where
    toJSON (YaiflItem{..}) = A.object $
      [ "level" A..= itemSeverity
      , "message" A..= itemMessage
      , "timestamp" A..= formatAsLogTime itemTime
      , "ns" A..= let f = T.intercalate "➤" (filter (/= T.empty) $ itemContext) in if T.empty == f then "" else "❬"<>f<>"❭"
      , "loc" A..= fmap reshapeFilename itemLoc
      ]
```
