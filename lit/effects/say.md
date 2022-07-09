# Yaifl.Say

This is just a riff on every `Teletype` example that seems to be standard with effect frameworks. It's slightly more involved than the logging effect because we have conditional saying as well as formatting to keep track of.

```haskell file=src/Yaifl/Core/Say.hs
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Say
  ( -- * Types
    MessageBuffer (..)
  , Saying(..)
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
  SetStyle :: Maybe PPTTY.AnsiStyle -> Saying m ()

data MessageBuffer = MessageBuffer
  { _msgBufBuffer :: [StyledDoc] -- ^ Current messages held before flushing.
  , _msgBufStyle :: Maybe PPTTY.AnsiStyle -- ^ Current formatting; 'Nothing' = plain.
  , _msgBufContext :: [StyledDoc] -- ^ Possibly nested prefixes before every message.
  }

<<say-helpers>>
<<interpret-say>>

<<say-functions>>
```

We define our `Effect` to take a pretty-printed document. This does mean we almost never use the effect directly, but still. We also hold onto some message context - a style and some additional "preface every entry with this" text. If we're using the `IO` interpretation, then we just leave the buffer blank.

```haskell id=say-helpers
blankMessageBuffer :: MessageBuffer
blankMessageBuffer = MessageBuffer [] Nothing []

makeEffect ''Saying
makeLenses ''MessageBuffer
```

## Interpreting SayDoc

Both interpreters do very similar things, with the difference being where the output ends up. We need a `MessageBuffer` to be present in the effect stack regardless to pre-process the doc by setting the style and amending any context.

```haskell id=interpret-say
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

<<interpret-say-pure>>
<<interpret-say-io>>
```

And we can then interpret a `SayDoc` by amending to the buffer:

```haskell id=interpret-say-pure
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

```

or by dumping straight to `stdout`:

```haskell id=interpret-say-io
runSayIO ::
  IOE :> es
  => PartialState s MessageBuffer es
  => Eff (Saying : es)
  ~> Eff es
runSayIO = zoom (buf @_ @MessageBuffer) . reinterpret \case
  SayDoc doc -> do
    r <- processDoc doc
    print r
```

## Actually saying things

And now we can write our `say` functions independent of whether we're in a pure or IO context. There's a handful of variations just because it's easier to do it all here.

```haskell id=say-functions
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
setStyle' :: 
  forall s es. 
  PartialState s MessageBuffer es
  => Maybe PPTTY.AnsiStyle -- ^ The updated style.
  -> Eff es ()
setStyle' s = buf @s @MessageBuffer % msgBufStyle .= s

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
```
