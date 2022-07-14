-- ~\~ language=Haskell filename=src/Yaifl/Core/Logger.hs
-- ~\~ begin <<lit/effects/logging.md|src/Yaifl/Core/Logger.hs>>[0] project://lit/effects/logging.md:6
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Logger where

import Language.Haskell.TH ( Loc(..) )
import Cleff.Trace ( trace, Trace )
import Cleff.Reader ( Reader, asks, local )
import GHC.Stack
import Data.Time ( getCurrentTime, UTCTime )
import qualified Data.Aeson as A
import qualified Data.Text as T
import Katip.Format.Time ( formatAsLogTime )
import Prelude hiding (Reader, asks, trace, local)

-- ~\~ begin <<lit/effects/logging.md|log-effect>>[0] project://lit/effects/logging.md:34
data MsgSeverity = Debug | Info | Warning | Error
  deriving stock (Eq, Ord, Enum, Bounded, Show)

data Log :: Effect where
  LogMsg :: Maybe Loc -> MsgSeverity -> Text -> Log m ()
  WithContext :: Text -> m a -> Log m a

makeEffect ''Log
-- ~\~ end
-- ~\~ begin <<lit/effects/logging.md|log-interpreters>>[0] project://lit/effects/logging.md:50
runAndIgnoreLogging ::
  Eff (Log : es)
  ~> Eff es
runAndIgnoreLogging = interpret \case
  LogMsg{} -> pass
  WithContext _ m -> toEff m

runLoggingAsTrace ::
  Reader [Text] :> es
  => IOE :> es
  => Eff (Log : es)
  ~> Eff (Trace : es)
runLoggingAsTrace = reinterpret \case
  LogMsg mbLoc sev msg -> do
    now <- liftIO getCurrentTime
    cxt <- asks reverse
    trace $ makeJSONObject now cxt sev mbLoc msg
  WithContext cxt m -> local (cxt:) (toEff m)

makeJSONObject :: UTCTime -> [Text] -> MsgSeverity -> Maybe Loc -> Text -> String
makeJSONObject now cxt sev mbLoc pl = decodeUtf8 $ A.encode $
  YaiflItem
  { itemSeverity = show sev
  , itemLoc = mbLoc
  , itemMessage = pl
  , itemTime = now
  , itemContext = cxt
  }
-- ~\~ end
-- ~\~ begin <<lit/effects/logging.md|log-functions>>[0] project://lit/effects/logging.md:102
logInternal ::
  HasCallStack
  => Log :> es
  => MsgSeverity
  -> Text
  -> Eff es ()
logInternal = logMsg (toLoc callStack)

debug ::
  HasCallStack
  => Log :> es
  => Text
  -> Eff es ()
debug = logInternal Debug

info ::
  HasCallStack
  => Log :> es
  => Text
  -> Eff es ()
info = logInternal Info

warn ::
  HasCallStack
  => Log :> es
  => Text
  -> Eff es ()
warn = logInternal Warning

err ::
  HasCallStack
  => Log :> es
  => Text
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
    toJSON (YaiflItem{..}) = A.object
      [ "level" A..= itemSeverity
      , "message" A..= itemMessage
      , "timestamp" A..= formatAsLogTime itemTime
      , "ns" A..= let f = T.intercalate "➤" (filter (/= T.empty) itemContext) in if T.empty == f then "" else "❬"<>f<>"❭"
      , "loc" A..= fmap reshapeFilename itemLoc
      ]
-- ~\~ end
-- ~\~ end
