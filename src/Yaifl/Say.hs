-- ~\~ language=Haskell filename=src/Yaifl/Say.hs
-- ~\~ begin <<lit/effects/say.md|src/Yaifl/Say.hs>>[0] project://lit/effects/say.md:6
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Say
  ( -- * Types
    MessageBuffer (..)
  -- * Smart constructors
  , blankMessageBuffer
  -- * Buffer modification
  , setStyle
  , say
  , sayLn
  , sayIf

  , msgBufBuffer
  )
where

import Cleff.State ( State, get, modify )
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Terminal as PPTTY
import Solitude

type StyledDoc = PP.Doc PPTTY.AnsiStyle

data Saying :: Effect where
  SayDoc :: StyledDoc -> Saying m ()

data MessageBuffer = MessageBuffer
  { _msgBufBuffer :: [StyledDoc] -- ^ Current messages held before flushing.
  , _msgBufStyle :: Maybe PPTTY.AnsiStyle -- ^ Current formatting; 'Nothing' = plain.
  , _msgBufContext :: [StyledDoc] -- ^ Possibly nested prefixes before every message.
  }

-- ~\~ begin <<lit/effects/say.md|say-helpers>>[0] project://lit/effects/say.md:48
blankMessageBuffer :: MessageBuffer
blankMessageBuffer = MessageBuffer [] Nothing []

makeEffect ''Saying
makeLenses ''MessageBuffer
-- ~\~ end
-- ~\~ begin <<lit/effects/say.md|interpret-say>>[0] project://lit/effects/say.md:60
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

-- ~\~ begin <<lit/effects/say.md|interpret-say-pure>>[0] project://lit/effects/say.md:79
class Has s t where
  buf :: Lens' s t

type PartialState s t es = (Has s t, State s :> es)

runSayPure ::
  forall s es. 
  PartialState s MessageBuffer es
  => Eff (Saying : es)
  ~> Eff es
runSayPure = zoom (buf @s @MessageBuffer) . reinterpret \case
  SayDoc doc -> do
    r <- processDoc doc
    modify (\s -> s & msgBufBuffer %~ (r:))

-- ~\~ end
-- ~\~ begin <<lit/effects/say.md|interpret-say-io>>[0] project://lit/effects/say.md:99
runSayIO ::
  IOE :> es
  => PartialState s MessageBuffer es
  => Eff (Saying : es)
  ~> Eff es
runSayIO = zoom (buf @_ @MessageBuffer) . reinterpret \case
  SayDoc doc -> do
    r <- processDoc doc
    print r
-- ~\~ end
-- ~\~ end

-- ~\~ begin <<lit/effects/say.md|say-functions>>[0] project://lit/effects/say.md:115
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

{-
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
