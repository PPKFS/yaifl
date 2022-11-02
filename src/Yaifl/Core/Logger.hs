{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Yaifl.Core.Logger where

import Language.Haskell.TH ( Loc(..) )
import GHC.Stack
import Data.Time ( getCurrentTime, UTCTime )
import qualified Data.Aeson as A
import qualified Data.Text as T
--import Katip.Format.Time ( formatAsLogTime )
import Yaifl.Core.Say ( Has(buf), PartialState )
import Effectful
import Effectful.TH
import Solitude hiding (local, asks, Reader)
import Effectful.Reader.Static
import Effectful.Dispatch.Dynamic
import Effectful.Optics

data MsgSeverity = Debug | Info | Warning | Error
  deriving stock (Eq, Ord, Enum, Bounded, Show)

data Log :: Effect where
  LogMsg :: Maybe Loc -> MsgSeverity -> Text -> Log m ()
  WithContext :: Text -> m a -> Log m a

newtype LogBuffer = LB [([Text], UTCTime, Maybe Loc, MsgSeverity, Text)]

makeEffect ''Log

runAndIgnoreLogging ::
  Eff (Log : es) a
  -> Eff es a
runAndIgnoreLogging = interpret $ \env -> \case
  LogMsg{} -> pass
  WithContext _ m -> localSeqUnlift env $ \unlift -> unlift m

runLoggingInternal ::
  forall s es a.
  Reader [Text] :> es
  => IOE :> es
  => PartialState s LogBuffer es
  => Eff (Log : es) a
  -> Eff es a
runLoggingInternal = interpret $ \env -> \case
  LogMsg mbLoc sev msg -> do
    now <- liftIO getCurrentTime
    cxt <- asks reverse
    buf %= (\(LB a) -> LB ((cxt, now, mbLoc, sev, msg):a))
  WithContext cxt m -> localSeqUnlift env $ \unlift -> local (cxt:) (unlift m)

runLoggingAsTrace ::
  Reader [Text] :> es
  => IOE :> es
  => Eff (Log : es) ()
  -> Eff es ()
runLoggingAsTrace = interpret $ \env -> \case
  LogMsg mbLoc sev msg -> do
    now <- liftIO getCurrentTime
    cxt <- asks reverse
    liftIO (trace (makeJSONObject now cxt sev mbLoc msg) pass)
  WithContext cxt m -> localSeqUnlift env $ \unlift -> local (cxt:) (unlift m)

makeJSONObject :: UTCTime -> [Text] -> MsgSeverity -> Maybe Loc -> Text -> String
makeJSONObject now cxt sev mbLoc pl = decodeUtf8 $ A.encode $
  YaiflItem
  { itemSeverity = show sev
  , itemLoc = mbLoc
  , itemMessage = pl
  , itemTime = now
  , itemContext = cxt
  }

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
      , "timestamp" A..= show @Text itemTime
      , "ns" A..= let f = T.intercalate "➤" (filter (/= T.empty) itemContext) in if T.empty == f then "" else "❬"<>f<>"❭"
      , "loc" A..= fmap reshapeFilename itemLoc
      ]
