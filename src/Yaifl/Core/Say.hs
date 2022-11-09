{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Say
  ( -- * Types
    MessageBuffer (..)
  , Saying(..)
  , Has(..)
  , PartialState
  -- * Smart constructors
  , blankMessageBuffer
  -- * Buffer modification
  , setStyle
  , say
  , sayLn
  , sayIf

  , runSayPure
  , runSayIO

  , msgBufBuffer
  )
where

import Solitude

import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Terminal as PPTTY
import Effectful.TH ( makeEffect )
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Optics (use, (.=))

type StyledDoc = PP.Doc PPTTY.AnsiStyle

data Saying :: Effect where
  SayDoc :: StyledDoc -> Saying m ()
  SetStyle :: Maybe PPTTY.AnsiStyle -> Saying m ()

data MessageBuffer = MessageBuffer
  { _msgBufBuffer :: [StyledDoc] -- ^ Current messages held before flushing.
  , _msgBufStyle :: Maybe PPTTY.AnsiStyle -- ^ Current formatting; 'Nothing' = plain.
  , _msgBufContext :: [StyledDoc] -- ^ Possibly nested prefixes before every message.
  }

blankMessageBuffer :: MessageBuffer
blankMessageBuffer = MessageBuffer [] Nothing []

makeEffect ''Saying
makeLenses ''MessageBuffer

processDoc ::
  forall s es.
  PartialState s MessageBuffer es
  => StyledDoc
  -> Eff es StyledDoc
processDoc msg = do
  (MessageBuffer _ style cxt) <- use @s buf
  -- if we have no context, we just monoid it.
  let joinOp = case cxt of
        [] -> (<>)
        _ -> (PP.<+>)
  return $ PP.hcat cxt `joinOp` maybe id PP.annotate style msg

class Has s t where
  buf :: Lens' s t

type PartialState s t es = (Has s t, State s :> es)

instance Has s s where
  buf = castOptic simple

runSayPure ::
  forall s es a.
  PartialState s MessageBuffer es
  => Eff (Saying : es) a
  -> Eff es a
runSayPure = interpret $ \_ -> \case
  SayDoc doc -> do
    r <- processDoc doc
    modify (\s -> s & buf % msgBufBuffer %~ (r:))
  SetStyle mbStyle -> setStyle' mbStyle

runSayIO ::
  forall s es a.
  IOE :> es
  => PartialState s MessageBuffer es
  => Eff (Saying : es) a
  -> Eff es a
runSayIO = interpret $ \_ -> \case
  SayDoc doc -> do
    r <- processDoc doc
    print r
  SetStyle mbStyle -> setStyle' mbStyle

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
  PartialState s MessageBuffer es
  => Maybe PPTTY.AnsiStyle -- ^ The updated style.
  -> Eff es ()
setStyle' s = buf % msgBufStyle .= s