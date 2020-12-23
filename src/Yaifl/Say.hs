module Yaifl.Say
  ( logMsg
  , addContext
  , removeContext
  , LoggingContext(..)
  , WithLogging
  , logToOutput
  , sayToOutput
  , SayOutput(..)
  , Say(..)
  , setStyle
  , say
  , sayLn
  , sayIf
  , Log
  , Severity(..)
  , prettyprintOutputToIO
  )
where

import           Yaifl.Prelude

import qualified Data.Text                     as T
import qualified Data.Text.Prettyprint.Doc     as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal
                                               as PPTTY
import Polysemy.State
import Polysemy.Output
import Polysemy
import Yaifl.PolysemyOptics

data LoggingContext = LoggingContext
  { _context      :: [Text]
  , _contextStyle :: PPTTY.AnsiStyle
  }

data Severity = Info | Debug | Error deriving Show
makeLenses ''LoggingContext

severityStyle :: Severity -> PPTTY.AnsiStyle
severityStyle Info  = PPTTY.colorDull PPTTY.Blue
severityStyle Debug = PPTTY.color PPTTY.Green
severityStyle Error = PPTTY.underlined <> PPTTY.color PPTTY.Red

data Log m k where
  LogMsg :: Severity -> Text -> Log m ()
  AddContext :: Text -> Maybe PPTTY.AnsiStyle -> Log m ()
  RemoveContext :: Log m ()

makeSem ''Log

logToOutput :: Member (State LoggingContext) r => Sem (Log ': r) a -> Sem (Output (PP.Doc PPTTY.AnsiStyle) ': r) a
logToOutput = reinterpret \case
  AddContext c s -> do
             context %= (c :)
             whenJust s (contextStyle .=)
  RemoveContext -> context %= (fromMaybe [] . viaNonEmpty tail)
  LogMsg sev t -> do
             LoggingContext cxt sty <- get
             let speltCxt = if null cxt
                   then ""
                   else "[" <> T.intercalate "." (reverse cxt) <> "]"
             output
               (PP.annotate (severityStyle sev <> PPTTY.bold)
                 (PP.pretty (T.concat ["[", show sev, "]"]))
               PP.<+> PP.annotate (PPTTY.bold <> sty) (PP.pretty speltCxt)
               PP.<+> PP.pretty t)

type WithLogging r = (Member (State LoggingContext) r, Member Log r)

data Say m k where
  Say ::Text -> Say m ()
  SetStyle :: Maybe PPTTY.AnsiStyle -> Say m ()

makeSem ''Say

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
sayIf iff t = when iff (say t){-
data Capitalisation = Capitalised | Uncapitalised
data Definiteness = Indefinite | Definite
data NameStyle = NameStyle Capitalisation Definiteness

type StyledDoc = PP.Doc PPTTY.AnsiStyle

defaultStyle :: NameStyle
defaultStyle = NameStyle Capitalised Definite
-}
