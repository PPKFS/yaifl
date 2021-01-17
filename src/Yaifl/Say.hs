module Yaifl.Say
  (
    LoggingContext(..)
  , Severity(..)
  , SayOutput(..)
  {-,, logToOutput
  , sayToOutput
  
  , Say(..)
  , setStyle
  , 
  , sayLn
  , sayIf
  , Log
  , 
  , prettyprintOutputToIO-}
  ) where

import           Yaifl.Prelude
import           Yaifl.Common

import           Control.Lens                   ( makeLenses )
import qualified Data.Text                     as T
import qualified Data.Text.Prettyprint.Doc     as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal
                                               as PPTTY

data LoggingContext = LoggingContext
  { _context      :: Text
  , _contextStyle :: PPTTY.AnsiStyle
  }
makeLenses ''LoggingContext

data Logging =
  Logging
    { _logError :: LoggingContext -> Text -> Base (),
      _logDebug :: LoggingContext -> Text -> Base ()
    }

logError :: (Has LoggingContext w, Has Logging w) => Text -> WorldT w Base ()
logError msg = do
  l <- view $ get @Logging $ unwrapWorld
  lp <- view $ get @LoggingContext
  lift $ _logError l lp msg

logDebug :: (Has LoggingContext w, Has Logging w) => Text -> WorldT w Base ()
logDebug msg = do
  l <- view $ get @Logging
  lp <- view $ get @LoggingContext
  lift $ _logDebug l lp msg

data Severity = Info | Debug | Error deriving Show
severityStyle :: Severity -> PPTTY.AnsiStyle
severityStyle Info  = PPTTY.colorDull PPTTY.Blue
severityStyle Debug = PPTTY.color PPTTY.Green
severityStyle Error = PPTTY.underlined <> PPTTY.color PPTTY.Red

newtype SayOutput = SayOutput { _unsay :: PP.Doc PPTTY.AnsiStyle } deriving (Semigroup, Monoid)
{-
addContext :: HasLoggingContext sig m => Text -> Maybe PPTTY.AnsiStyle -> m ()
addContext c s = do
  context %= (c :)
  whenJust s (contextStyle .=)

removeContext :: HasLoggingContext sig m => m ()
removeContext = context %= (fromMaybe [] . viaNonEmpty tail)

logMsg :: WithLogging sig m => Severity -> Text -> m ()
logMsg sev t = do
  LoggingContext cxt sty <- get @LoggingContext
  let speltCxt = if null cxt then "" else "[" <> T.intercalate "." (reverse cxt) <> "]"
  tell @(PP.Doc PPTTY.AnsiStyle)
    (      PP.annotate (severityStyle sev <> PPTTY.bold)
                       (PP.pretty (T.concat ["[", show sev, "]"]))
    PP.<+> PP.annotate (PPTTY.bold <> sty) (PP.pretty speltCxt)
    PP.<+> PP.pretty t <> "\n"
    )

say :: Has (Writer SayOutput) sig m => Text -> m ()
say t = tell @SayOutput $ SayOutput $ PP.pretty t

sayLn :: Has (Writer SayOutput) sig m => Text -> m ()
sayLn t = say (t <> "\n")
-}
{-


data Say m k where
  Say ::Text -> Say m ()
  SetStyle :: Maybe PPTTY.AnsiStyle -> Say m ()
newtype SayOutput = SayOutput { _unsay :: PP.Doc PPTTY.AnsiStyle }
sayToOutput :: Sem (Say ': r) a -> Sem (Output (PP.Doc PPTTY.AnsiStyle) ': r) a
sayToOutput = reinterpret \case
  Say t -> output $ (PP.pretty t)

prettyprintOutputToIO :: Member (Embed IO) r => Sem (Output (PP.Doc PPTTY.AnsiStyle) ': r) a -> Sem r a
prettyprintOutputToIO = interpret $ \case
  Output o -> embed $ PPTTY.putDoc $ o <> "\n"

sayLn :: Member Say r => Text -> Sem r ()
sayLn t = say (t <> "\n")

sayIf :: Member Say r => Bool -> Text -> Sem r ()
sayIf iff t = when iff (say t)
data Capitalisation = Capitalised | Uncapitalised
data Definiteness = Indefinite | Definite
data NameStyle = NameStyle Capitalisation Definiteness

type StyledDoc = PP.Doc PPTTY.AnsiStyle

defaultStyle :: NameStyle
defaultStyle = NameStyle Capitalised Definite
-}
