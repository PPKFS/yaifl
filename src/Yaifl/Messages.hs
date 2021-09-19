{-|
Module      : Yaifl.Messages
Description : Printing messages to the say (regular output) and logging buffers.
Copyright   : (c) Avery, 2021
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}
module Yaifl.Messages
  ( -- * Types
    MessageBuffer (..)
  , BufferTypes (..)
  , HasBuffer

  -- * Smart constructors
  , emptyMessageBuffer

  -- * Lenses
  , bufferL

  -- * Buffer modification
  , setStyle
  , setSayStyle
  , say
  , sayLn
  , sayIf
  , logInfo
  , logError
  , logVerbose
  , addLogContext
  , popLogContext

  -- ** Flushing
  , flushBufferToStdOut
  , flushBufferToText

  )
where

import Control.Lens
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY
import Relude

-- | A type d contains (some number of) buffers, indexed by a phantom type.
class HasBuffer d p where
  -- | Lens for the buffer.
  bufferL :: Proxy p -> Lens' d MessageBuffer

type StyledDoc = PP.Doc PPTTY.AnsiStyle

-- | A storage for messages along with a current printing style, contexts
-- | (for logging etc.) and whether things should be hidden (also for logging).
data MessageBuffer = MessageBuffer
  { _msgBufBuffer :: [StyledDoc] -- ^ Current messages held before flushing.
  , _msgBufStyle :: Maybe PPTTY.AnsiStyle -- ^ Current formatting; 'Nothing' = plain.
  , _msgBufContext :: [StyledDoc] -- ^ Possibly nested prefixes before every message.
  , _msgBufPrintLevel :: LogLevel -> Bool -- ^ Should we ignore this level of message?
  }
-- | Verbosity of logging messages.
data LogLevel = VerboseLevel | InfoLevel | ErrorLevel

-- | Message buffer with nothing in it and no formatting.
emptyMessageBuffer :: MessageBuffer
emptyMessageBuffer = MessageBuffer [] Nothing [] (const True)

makeLenses ''MessageBuffer

lb :: Proxy 'LogBuffer
lb = Proxy

sb :: Proxy 'SayBuffer
sb = Proxy

-- | Right now I only need 2 kinds of buffer, a regular output one and a log.
data BufferTypes = LogBuffer | SayBuffer

sayInternal
  :: (HasBuffer d p)
  => Proxy p
  -> StyledDoc
  -> d
  -> d
sayInternal prox msg w = w & bufferL prox . msgBufBuffer %~ (:)
    (sayContext buf `joinOp` maybe id PP.annotate style msg)
  where
    buf = w ^. bufferL prox
    style = buf ^. msgBufStyle
    cxt = buf ^. msgBufContext
    joinOp = case cxt of
     [] -> (<>)
     _ -> (PP.<+>)

sayContext
  :: MessageBuffer
  -> StyledDoc
sayContext = PP.hcat . _msgBufContext

-- | Print @message@ to the regular @say@ buffer.
say
  :: (HasBuffer w 'SayBuffer)
  => Text -- ^ Message.
  -> w
  -> w
say = sayInternal sb . PP.pretty

-- | Print @message@ to the say buffer with a newline.
sayLn
  :: (HasBuffer w 'SayBuffer)
  => Text -- ^ Message.
  -> w
  -> w
sayLn a = say (a <> "\n")

-- | Conditionally print @message@ to the say buffer.
sayIf
  :: (HasBuffer w 'SayBuffer)
  => Bool -- ^ Condition to evaluate.
  -> Text -- ^ Message.
  -> w
  -> w
sayIf True = say
sayIf False = const id

-- | Print @message@ to the logging buffer.
log
  :: (HasBuffer w 'LogBuffer)
  => LogLevel -- ^ Level of logging for this message.
  -> Text -- ^ Message.
  -> w
  -> w
log logLevel message w
    | shouldPrint lb w logLevel = (sayInternal lb . PP.pretty) message w
    | otherwise = w

shouldPrint
  :: (HasBuffer w p)
  => Proxy p
  -> w -- ^
  -> LogLevel -- ^
  -> Bool
shouldPrint prox = view (bufferL prox . msgBufPrintLevel)

-- | Print @message@ to the logging buffer with a newline.
logLn
  :: (HasBuffer w 'LogBuffer)
  => LogLevel -- ^ Level of logging for this message.
  -> Text -- ^ Message.
  -> w
  -> w
logLn logLevel message = log logLevel (message <> "\n")

-- | Print @message@ to the logging buffer with a newline and @Info@ prefix.
logInfo
  :: (HasBuffer w 'LogBuffer)
  => Text -- ^ Message.
  -> w
  -> w
logInfo = withLogPrefix InfoLevel PPTTY.Blue "Msg"

-- | Print @message@ to the logging buffer with a newline and @Debug@ prefix.
logVerbose
  :: (HasBuffer w 'LogBuffer)
  => Text -- ^ Message.
  -> w
  -> w
logVerbose = withLogPrefix VerboseLevel PPTTY.Green "Dbg"

-- | Print @message@ to the logging buffer with a newline and @Error@ prefix.
logError
  :: (HasBuffer w 'LogBuffer)
  => Text -- ^ Message.
  -> w
  -> w
logError = withLogPrefix VerboseLevel PPTTY.Red "Err"

-- | Print @message@ to the logging buffer with a (local) prefix.
-- | This will respect the existing context and styles, if applicable.
withLogPrefix
  :: (HasBuffer w 'LogBuffer)
  => LogLevel
  -> PPTTY.Color
  -> Text -- ^ Message prefix.
  -> Text -- ^ Message itself.
  -> w
  -> w
withLogPrefix logLevel colour prefix message = execState $ do
  oldBuf <- use $ bufferL lb . msgBufContext
  -- append the logging prefix to the context and make it pretty
  bufferL lb . msgBufContext %= (logContextPrefix colour prefix :)
  -- update
  modify $ logLn logLevel message
  -- restore
  bufferL lb . msgBufContext .= oldBuf

logContextPrefix
  :: PP.Pretty a
  => PPTTY.Color
  -> a
  -> PP.Doc PPTTY.AnsiStyle
logContextPrefix colour prefix = PP.surround
    (PP.annotate
        (PPTTY.colorDull colour <> PPTTY.bold)
        (PP.pretty prefix))
        (bf "[") (bf "]")
    where bf = PP.annotate PPTTY.bold
-- | Update the style of a message buffer. Setting to 'Just' overwrites the style,
-- | whereas 'Nothing' will remove it. This will not affect previous messages.
setStyle
  :: (HasBuffer w p)
  => Proxy p
  -> Maybe PPTTY.AnsiStyle -- ^ The updated style.
  -> w
  -> w
setStyle prox = set $ bufferL prox . msgBufStyle

-- | Update the style of the say buffer. Setting to 'Just' overwrites the style,
-- | whereas 'Nothing' will remove it. This will not affect previous messages.
setSayStyle
  :: (HasBuffer w 'SayBuffer)
  => Maybe PPTTY.AnsiStyle
  -> w
  -> w
setSayStyle = setStyle sb

-- | Update the style of the logging buffer. Setting to 'Just' overwrites
-- the style, whereas 'Nothing' will remove it.
-- This will not affect previous messages.
setLogStyle
  :: (HasBuffer w 'LogBuffer)
  => Maybe PPTTY.AnsiStyle
  -> w
  -> w
setLogStyle = setStyle lb

-- | Add some context to the log buffer.
addLogContext
  :: (HasBuffer w 'LogBuffer)
  => Text
  -> w
  -> w
addLogContext cxt = bufferL lb . msgBufContext %~ (logContextPrefix PPTTY.White cxt :)

-- | Remove the last layer of context from the log buffer
popLogContext
  :: (HasBuffer w 'LogBuffer)
  => w
  -> w
popLogContext w = w & bufferL lb . msgBufContext %~ drop 1

-- | Clear a message buffer and return the container (with a clean buffer) and the string
-- with all formatting (e.g. ANSI colour codes) removed.
flushBufferToText
  :: (HasBuffer w p)
  => Proxy p
  -> w
  -> (Text, w)
flushBufferToText prox = runState $ do
  -- take it down and flip it around
  msgList <- use $ bufferL prox . msgBufBuffer . reversed
  bufferL prox . msgBufBuffer .= []
  return $ (mconcat . map show) msgList

-- | Clear a message buffer and return the container (with a clean buffer)
-- with all formatting (e.g. ANSI colour codes) *included*.
flushBufferToStdOut
  :: (MonadIO m, HasBuffer w p)
  => Proxy p
  -> w
  -> m w
flushBufferToStdOut prox w = do
  let output' = (PPTTY.putDoc (comboBuffer w prox), w & bufferL prox . msgBufBuffer .~ [])
  liftIO $ fst output'
  return (snd output')
  where
    comboBuffer d' p' = PP.hcat $ reverse $ d' ^. bufferL p' . msgBufBuffer
