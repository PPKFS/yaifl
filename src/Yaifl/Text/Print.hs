
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Text.Print
  ( -- * Types
    MessageBuffer (..)
  , Print(..)
  , Has(..)
  , PartialState
  -- * Smart constructors
  , blankMessageBuffer
  -- * Buffer modification
  , setStyle
  , printText
  , printLn
  , printIf
  , runPrintPure
  , runPrintIO
  )
where

import Solitude

import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Terminal as PPTTY
import Effectful.TH ( makeEffect )
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Optics (use, (.=))

type StyledDoc = PP.Doc PPTTY.AnsiStyle

data MessageContext = MessageContext
  { messageFromRule :: Text
  ,  :: Bool
  , shouldLinebreak :: Bool
  } deriving stock (Show, Ord, Generic, Eq)

data Print :: Effect where
  PrintDoc :: MessageContext -> StyledDoc -> Print m ()
  SetStyle :: Maybe PPTTY.AnsiStyle -> Print m ()

makeEffect ''Print

data MessageBuffer = MessageBuffer
  { buffer :: [StyledDoc] -- ^ Current messages held before flushing.
  , lastMessageContext :: MessageContext -- ^ some metadata about the last printed message, to deal with pbreaks and lines
  , style :: Maybe PPTTY.AnsiStyle -- ^ Current formatting; 'Nothing' = plain.
  , context :: [StyledDoc] -- ^ Possibly nested prefixes before every message.
  } deriving stock (Show, Generic)

makeFieldLabelsNoPrefix ''MessageBuffer

blankMessageBuffer :: MessageBuffer
blankMessageBuffer = MessageBuffer [] (MessageContext "" False False) Nothing []

processDoc ::
  forall s es.
  PartialState s MessageBuffer es
  => StyledDoc
  -> Eff es StyledDoc
processDoc msg = do
  (MessageBuffer _ _ style cxt) <- use @s buf
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

runPrintPure ::
  forall s es a.
  PartialState s MessageBuffer es
  => Eff (Print : es) a
  -> Eff es a
runPrintPure = interpret $ \_ -> \case
  PrintDoc metadata doc -> do
    buf % #lastMessageContext .= metadata
    r <- processDoc doc
    modify (\s -> s & buf % (#buffer @(Lens' MessageBuffer [StyledDoc])) %~ (r:))
  SetStyle mbStyle -> setStyle' mbStyle

runPrintIO ::
  forall s es a.
  IOE :> es
  => PartialState s MessageBuffer es
  => Eff (Print : es) a
  -> Eff es a
runPrintIO = interpret $ \_ -> \case
  PrintDoc metadata doc -> do
    r <- processDoc doc
    buf % #lastMessageContext .= metadata
    print r
  SetStyle mbStyle -> setStyle' mbStyle

-- | Print a string (well, Text).
printText ::
  Print :> es
  => Text -- ^ Message.
  -> Eff es ()
printText t = do
  checkForLinebreaking t
  printDoc . PP.pretty $ t

-- we need to add a pbreak if:
-- one was explicitly called for
-- we have finished the rule which the last printed text was from
-- which means every piece of text should track also where it came from
-- and also what that means for the following formatting
-- |
checkForLinebreaking :: Text -> Eff es ()
checkForLinebreaking t = do
  lsc <- use (buf % #lastMessageContext)

  --
  -- if we have a pending line or paragraph break then push it
  --

-- | Print @message@ with a newline.
printLn ::
  Print :> es
  => Text -- ^ Message.
  -> Eff es ()
printLn a = printText (a <> "\n")

-- | Conditionally Print @message@.
printIf ::
  Print :> es
  => Bool -- ^ Condition to evaluate.
  -> Text -- ^ Message.
  -> Eff es ()
printIf True = printText
printIf False = const pass

-- | Update the style of a message buffer. Setting to 'Just' overwrites the style,
-- | whereas 'Nothing' will remove it. This will not affect previous messages.
setStyle' ::
  PartialState s MessageBuffer es
  => Maybe PPTTY.AnsiStyle -- ^ The updated style.
  -> Eff es ()
setStyle' s = buf % (#style @(Lens' MessageBuffer (Maybe PPTTY.AnsiStyle))) .= s