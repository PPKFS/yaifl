-- ~\~ language=Haskell filename=src/Yaifl/Say.hs
-- ~\~ begin <<lit/effects/say.md|src/Yaifl/Say.hs>>[0]
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Cleff.Plugin #-}

module Yaifl.Say
  ( -- * Types
    MessageBuffer (..)
  -- * Smart constructors
  , emptyMessageBuffer
  -- * Buffer modification
  , setStyle
  , say
  , sayLn
  , sayIf
  )
where

import Cleff
import Cleff.State hiding (zoom)

import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Terminal as PPTTY
import Solitude

type StyledDoc = PP.Doc PPTTY.AnsiStyle

data Saying :: Effect where
  SayDoc :: StyledDoc -> Saying m ()

head1 :: [a] -> a
head1 [] = error ""
head1 (x:_) = x

data MessageBuffer = MessageBuffer
  { _msgBufBuffer :: [StyledDoc] -- ^ Current messages held before flushing.
  , _msgBufStyle :: Maybe PPTTY.AnsiStyle -- ^ Current formatting; 'Nothing' = plain.
  , _msgBufContext :: [StyledDoc] -- ^ Possibly nested prefixes before every message.
  }

-- ~\~ begin <<lit/effects/say.md|say-helpers>>[0]
-- | Message buffer with nothing in it and no formatting.
emptyMessageBuffer :: MessageBuffer
emptyMessageBuffer = MessageBuffer [] Nothing []

makeEffect ''Saying
makeLenses ''MessageBuffer
-- ~\~ end
-- ~\~ begin <<lit/effects/say.md|interpret-say>>[0]
processDoc ::
  State MessageBuffer :> es
  => StyledDoc
  -> Eff es StyledDoc
processDoc msg = do
  (MessageBuffer _ style cxt) <- get
  -- if we have no context, we just monoid it.
  let joinOp = case cxt of
        [] -> (<>)
        _ -> (PP.<+>)
  return $ PP.hcat cxt `joinOp` maybe id PP.annotate style msg

-- ~\~ begin <<lit/effects/say.md|interpret-say-pure>>[0]

class Has s t where
  buf :: Lens' s t

type PartialState s t es = (Has s t, State s :> es)

zoom :: State t :> es => Lens' t s -> Eff (State s : es) ~> Eff es
zoom field = interpret \case
  Get     -> gets (^. field)
  Put s   -> modify (& field .~ s)
  State f -> state \t -> let (a, !s) = f (t ^. field) in (a, t & field .~ s)
{-# INLINE zoom #-}

runSayPure ::
  forall s es. 
  PartialState s MessageBuffer es
  => Proxy s -- ^ we need this to avoid ambiguous types as `s` vanishes
  -> Eff (Saying : es)
  ~> Eff es
runSayPure _ = zoom @s @_ @MessageBuffer buf . reinterpret \case
  SayDoc doc -> do
    r <- processDoc doc
    modify (\s -> s & msgBufBuffer %~ (r:))

-- ~\~ end
-- ~\~ begin <<lit/effects/say.md|interpret-say-io>>[0]
runSayIO ::
  IOE :> es
  => Eff (Saying : es)
  ~> Eff es
runSayIO = interpretIO \case
  SayDoc doc -> print doc
-- ~\~ end
-- ~\~ end

-- ~\~ begin <<lit/effects/say.md|say-functions>>[0]

-- | Say a string (well, Text).
say :: 
  Saying :> es 
  => Text -- ^ Message.
  -> Eff es ()
say = sayDoc . PP.pretty

-- | Say @message@ with a newline.
sayLn :: 
  Saying :> es 
  => Text -- ^ Message.
  -> Eff es ()
sayLn a = say (a <> "\n")

-- | Conditionally say @message@.
sayIf :: 
  Saying :> es 
  => Bool -- ^ Condition to evaluate.
  -> Text -- ^ Message.
  -> Eff es ()
sayIf True = say
sayIf False = const pass

-- | Update the style of a message buffer. Setting to 'Just' overwrites the style,
-- | whereas 'Nothing' will remove it. This will not affect previous messages.
setStyle :: 
  forall s es. 
  PartialState s MessageBuffer es
  => Maybe PPTTY.AnsiStyle -- ^ The updated style.
  -> Eff es ()
setStyle s = buf @s @MessageBuffer % msgBufStyle .= s

(.=) :: State s :> es => Lens' s a -> a -> Eff es ()
(.=) = undefined
{-
-- | Clear a message buffer and return the container (with a clean buffer) and the string
-- with all formatting (e.g. ANSI colour codes) removed.
flushBufferToText :: 
  Saying :> es
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
flushBufferToStdOut :: 
  MonadIO m
  => HasBuffer w p 
  => Proxy p
  -> w
  -> m w
flushBufferToStdOut prox w = do
  let output' = (PPTTY.putDoc (comboBuffer w prox), w & bufferL prox % msgBufBuffer .~ [])
  liftIO $ fst output'
  return (snd output')
  where
    comboBuffer d' p' = PP.hcat $ reverse $ d' ^. bufferL p' % msgBufBuffer
-}
-- ~\~ end
-- ~\~ end
