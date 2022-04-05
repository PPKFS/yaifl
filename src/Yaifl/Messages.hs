{-|
Module      : Yaifl.Messages
Description : Printing messages to the say (regular output) and logging buffers.
Copyright   : (c) Avery, 2021
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE TemplateHaskell #-}

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
  --, setStyle
  , setSayStyle
  , say
  , sayLn
  , sayIf

  -- ** Flushing
  , flushBufferToStdOut
  , flushBufferToText

  )
where

import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY
import Solitude

-- | A type d contains (some number of) buffers, indexed by a phantom type.
class HasBuffer d p where
  -- | Lens for the buffer.
  bufferL :: Proxy p -> Lens' d MessageBuffer

type StyledDoc = PP.Doc PPTTY.AnsiStyle

-- | A storage for messages along with a current printing style, contexts, etc. Kinda moot now I have katip?
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

sb :: Proxy 'SayBuffer
sb = Proxy

-- | Right now I only need 1 kind of buffer, a regular output one.
data BufferTypes = SayBuffer

sayInternal
  :: (HasBuffer w p)
  => MonadState w m
  => Proxy p
  -> StyledDoc
  -> m ()
sayInternal prox msg = do
  buf <- use $ bufferL prox
  let
    style = buf ^. msgBufStyle
    cxt = buf ^. msgBufContext
    joinOp = case cxt of
     [] -> (<>)
     _ -> (PP.<+>)
  bufferL prox % msgBufBuffer %= (:)
    (sayContext buf `joinOp` maybe id PP.annotate style msg)
    
sayContext
  :: MessageBuffer
  -> StyledDoc
sayContext = PP.hcat . _msgBufContext

-- | Print @message@ to the regular @say@ buffer.
say
  :: (HasBuffer w 'SayBuffer)
  => MonadState w m
  => Text -- ^ Message.
  -> m ()
say = sayInternal sb . PP.pretty

-- | Print @message@ to the say buffer with a newline.
sayLn
  :: (HasBuffer w 'SayBuffer)
  => MonadState w m
  => Text -- ^ Message.
  -> m ()
sayLn a = say (a <> "\n")

-- | Conditionally print @message@ to the say buffer.
sayIf
  :: (HasBuffer w 'SayBuffer)
  => MonadState w m
  => Bool -- ^ Condition to evaluate.
  -> Text -- ^ Message.
  -> m ()
sayIf True = say
sayIf False = const pass

-- | Update the style of a message buffer. Setting to 'Just' overwrites the style,
-- | whereas 'Nothing' will remove it. This will not affect previous messages.
setStyle
  :: (HasBuffer w p)
  => MonadState w m
  => Proxy p
  -> Maybe PPTTY.AnsiStyle -- ^ The updated style.
  -> m ()
setStyle prox s = bufferL prox % msgBufStyle .= s

-- | Update the style of the say buffer. Setting to 'Just' overwrites the style,
-- | whereas 'Nothing' will remove it. This will not affect previous messages.
setSayStyle
  :: (HasBuffer w 'SayBuffer)
  => MonadState w m
  => Maybe PPTTY.AnsiStyle
  -> m ()
setSayStyle = setStyle sb

-- | Clear a message buffer and return the container (with a clean buffer) and the string
-- with all formatting (e.g. ANSI colour codes) removed.
flushBufferToText
  :: (HasBuffer w p)
  => Proxy p
  -> w
  -> (Text, w)
flushBufferToText prox = runState $ do
  -- take it down and flip it around
  msgList <- use $ bufferL prox % msgBufBuffer % reversed
  bufferL prox % msgBufBuffer .= []
  return $ (mconcat . map show) msgList

-- | Clear a message buffer and return the container (with a clean buffer)
-- with all formatting (e.g. ANSI colour codes) *included*.
flushBufferToStdOut
  :: (MonadIO m, HasBuffer w p)
  => Proxy p
  -> w
  -> m w
flushBufferToStdOut prox w = do
  let output' = (PPTTY.putDoc (comboBuffer w prox), w & bufferL prox % msgBufBuffer .~ [])
  liftIO $ fst output'
  return (snd output')
  where
    comboBuffer d' p' = PP.hcat $ reverse $ d' ^. bufferL p' % msgBufBuffer
