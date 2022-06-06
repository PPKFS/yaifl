-- ~\~ language=Haskell filename=src/Yaifl/Logger.hs
-- ~\~ begin <<lit/effects/logging.md|src/Yaifl/Logger.hs>>[0] project://lit/effects/logging.md:6
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

-- ~\~ begin <<lit/effects/logging.md|log-effect>>[0] project://lit/effects/logging.md:34
data MsgSeverity = Debug | Info | Warning | Error
  deriving stock (Eq, Ord, Enum, Bounded, Show)
  deriving (Display) via (ShowInstance MsgSeverity)

data Log :: Effect where
  LogMsg :: Maybe Loc -> MsgSeverity -> Text -> Log m () 
  WithContext :: Text -> m a -> Log m ()

makeEffect ''Log
-- ~\~ end
-- ~\~ begin <<lit/effects/logging.md|log-interpreters>>[0] project://lit/effects/logging.md:50
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
-- ~\~ end
-- ~\~ begin <<lit/effects/logging.md|log-functions>>[0] project://lit/effects/logging.md:102
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
-- ~\~ end
-- ~\~ begin <<lit/effects/logging.md|callstack-log>>[0] project://lit/effects/logging.md:84
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
-- ~\~ end
-- ~\~ begin <<lit/effects/logging.md|log-item>>[0] project://lit/effects/logging.md:146

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
-- ~\~ end
-- ~\~ end
