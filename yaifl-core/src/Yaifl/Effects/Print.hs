
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Yaifl.Effects.Print
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Text output and message formatting system for interactive fiction.

This module provides a comprehensive printing system for game messages with:

- `Print` effect: Core printing functionality for game text output
- `MessageBuffer`: Accumulates and formats messages before display
- `MessageContext`: Tracks formatting context and paragraph breaks
- `MessageAnnotation`: Rich text styling (colors, bold, italics, underline)
- `StyledDoc`: Formatted document representation using Prettyprinter

Key features:
- Context-aware message formatting with automatic paragraph breaks
- Rich text styling with colors and font effects
- Message buffering and flushing system
- Rule-based context tracking for consistent output
- Support for both pure (testing) and IO-based printing
-}

module Yaifl.Effects.Print
  ( -- * Core Types
    MessageBuffer (..)
  , MessageContext(..)
  , MessageAnnotation(..)
  , Print(..)
  , Has(..)
  , PartialState
  , StyledDoc

  -- * Style Types
  , Bold(..)
  , Italics(..)
  , Underlined(..)
  , Colour(..)
  , Color

  -- * Smart Constructors
  , blankMessageBuffer

  -- * Buffer Operations
  -- | Set the current message style.
  -- Updates the formatting style used for subsequent messages.
  , setStyle
  -- | Modify the message buffer.
  -- Apply a transformation function to the current message buffer state.
  , modifyBuffer

  -- * Context Management
  , runOnParagraph
  , runOnLookingParagraph

  -- * Printing Functions
  , printText
  , printLn
  , printIf

  -- * Effect Interpreters
  , runPrintPure
  , runPrintIO

  -- * Style Functions
  , bold
  , toHex
  , fromARGB
  , fromRGB
  , colour
  , withStyle
  )
where

import Yaifl.Prelude

import qualified Prettyprinter as PP
import Effectful.TH ( makeEffect )
import Effectful.Dispatch.Dynamic (interpret)
import qualified Data.Text as T
import Data.Bits
import Data.Ix
import Numeric (showHex)
import Data.Ord (clamp)

type StyledDoc style = PP.Doc style

-- | ARGB color representation using a 32-bit word.
-- The color is stored as 0xAARRGGBB where AA is alpha, RR is red, GG is green, BB is blue.
newtype Colour = Colour { toWord32 :: Word32 }
  deriving stock (Generic)
  deriving newtype (Show, Read, Eq, Ord, Bits, FiniteBits, Num, Enum, Bounded, Ix, Real, Integral)

type Color = Colour

-- | Convert a color to its hexadecimal representation.
-- Returns a text string in the format "AARRGGBB" where AA is alpha, RR is red,
-- GG is green, and BB is blue (e.g., "80FF0000" for semi-transparent red).
toHex :: Colour -> Text
toHex = fromString . flip showHex "" . toWord32

-- | Create a color from ARGB components.
-- Each component should be in the range 0-255.
fromARGB :: Word8 -> Word8 -> Word8 -> Word8 -> Colour
fromARGB a r g b = (fromIntegral a `shiftL` 24) .|. (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

-- | Create an opaque color from RGB components.
-- Equivalent to @fromARGB 0xFF r g b@.
-- Each component should be in the range 0-255.
fromRGB :: Word8 -> Word8 -> Word8 -> Colour
fromRGB = fromARGB 0xFF

-- | Create a colour from ARGB components specified as floating-point values in the range 0-1.
-- Each component is automatically clamped to the valid range and scaled to 0-255.
-- This is a convenience function for when working with normalised colour values.
fromARGBFloat :: RealFrac a => RealFrac r => RealFrac g => RealFrac b => a -> r -> g -> b -> Colour
fromARGBFloat a r g b = fromARGB (clampScale a) (clampScale r) (clampScale g) (clampScale b)
  where
    clampScale :: RealFrac x => x -> Word8
    clampScale = round . (/255) . clamp (0, 1)

data Bold = Bold
  deriving stock (Eq, Ord, Show, Generic)

data Italics = Italics
  deriving stock (Eq, Ord, Show, Generic)

data Underlined = Underlined
  deriving stock (Eq, Ord, Show, Generic)

data MessageAnnotation = MessageAnnotation
    { foregroundAnnotation  :: Maybe Colour -- ^ Set the foreground color, or keep the old one.
    , backgroundAnnotation  :: Maybe Colour -- ^ Set the background color, or keep the old one.
    , boldAnnotation :: Maybe Bold               -- ^ Switch on boldness, or don’t do anything.
    , italics :: Maybe Italics         -- ^ Switch on italics, or don’t do anything.
    , underlined :: Maybe Underlined         -- ^ Switch on underlining, or don’t do anything.
    } deriving stock (Eq, Ord, Show, Generic)

instance Monoid MessageAnnotation where
  mempty = MessageAnnotation Nothing Nothing Nothing Nothing Nothing

instance Semigroup MessageAnnotation where
  (<>) m1 m2 = MessageAnnotation
    { foregroundAnnotation = (foregroundAnnotation m1) <|> foregroundAnnotation m2
    , backgroundAnnotation = (backgroundAnnotation m1) <|> backgroundAnnotation m2
    , boldAnnotation = (boldAnnotation m1) <|> boldAnnotation m2
    , italics = (italics m1) <|> italics m2
    , underlined = (underlined m1) <|> underlined m2
    }

colour :: Colour -> MessageAnnotation
colour c = mempty { foregroundAnnotation = Just c }

bold :: MessageAnnotation
bold = mempty { boldAnnotation = Just Bold }

-- | Context information about the current message being processed.
-- Tracks rule origin, paragraph state, and formatting requirements for consistent output.
data MessageContext = MessageContext
  { messageFromRule :: Text -- ^ The rule that generated this message.
  , runningOnParagraph :: Bool -- ^ Whether this message is part of a paragraph context.
  , shouldPrintLinebreak :: Bool -- ^ Whether a line break should be added after this message.
  , runningOnLookingParagraph :: Bool -- ^ Whether this message is part of a "looking" action context.
                                        -- When 'True', prevents paragraph breaks between different rules and forces
                                        -- single line breaks, creating compact room descriptions.
  , shouldPrintPbreak :: Bool -- ^ Whether a paragraph break should be added before the next message.
  , lastPrint :: Text -- ^ The text of the last printed message.
  } deriving stock (Show, Ord, Generic, Eq)

data Print :: Effect where
  ModifyBuffer :: (MessageBuffer -> MessageBuffer) -> Print m (MessageBuffer)
  GetBuffer :: Print m MessageBuffer
  PrintDoc :: Maybe MessageContext -> StyledDoc MessageAnnotation-> Print m ()
  SetStyle :: Maybe MessageAnnotation -> Print m ()

-- | Message buffer that accumulates formatted messages before display.
-- Tracks the current state of message formatting including style, context, and rule information.
data MessageBuffer = MessageBuffer
  { buffer :: [StyledDoc MessageAnnotation] -- ^ Current messages held before flushing.
  , lastMessageContext :: MessageContext -- ^ Metadata about the last printed message, used for paragraph breaks and line spacing.
  , style :: Maybe MessageAnnotation -- ^ Current formatting style; 'Nothing' indicates plain text.
  , context :: [StyledDoc MessageAnnotation] -- ^ Possibly nested prefixes that appear before every message.
  , ruleContext :: Text -- ^ The currently executing rule that is generating messages.
  } deriving stock (Show, Generic)

makeEffect ''Print
makeFieldLabelsNoPrefix ''MessageBuffer
makeFieldLabelsNoPrefix ''MessageContext

-- | Create an empty message buffer with default settings.
-- Initializes with no messages, default context, no styling, and empty rule context.
blankMessageBuffer :: MessageBuffer
blankMessageBuffer = MessageBuffer [] (MessageContext "¬¬¬" False False False False "") Nothing [] ""

processDoc ::
  forall s es.
  PartialState s (MessageBuffer) es
  => StyledDoc MessageAnnotation
  -> Eff es (StyledDoc MessageAnnotation)
processDoc msg = do
  (MessageBuffer _ _ style cxt _) <- use @s buf
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

-- | Run a 'Print' effect purely, accumulating messages in a buffer.
-- This interpreter is suitable for testing and scenarios where IO output is not desired.
-- Messages are stored in the buffer and can be retrieved later for inspection.
runPrintPure ::
  forall s es a.
  PartialState s (MessageBuffer) es
  => Eff (Print : es) a
  -> Eff es a
runPrintPure = interpret $ \_ -> \case
  PrintDoc mbMetadata doc -> do
    r <- processDoc doc
    modify (\s -> s & buf % (#buffer @(Lens' (MessageBuffer) [StyledDoc MessageAnnotation])) %~ (r:))
    whenJust mbMetadata $ \metadata -> modify (\s -> s & buf % #lastMessageContext @(Lens' (MessageBuffer) MessageContext) .~ metadata)
  SetStyle mbStyle -> setStyle' mbStyle
  ModifyBuffer f -> do
    modify (\s -> s & buf %~ f)
    use buf
  GetBuffer -> use buf

-- | Run a 'Print' effect with IO output.
-- This interpreter prints messages directly to standard output using 'print'.
-- Suitable for production use where immediate console output is desired.
runPrintIO ::
  forall s es a.
  IOE :> es
  => PartialState s (MessageBuffer) es
  => Eff (Print : es) a
  -> Eff es a
runPrintIO = interpret $ \_ -> \case
  PrintDoc mbMetadata doc -> do
    r <- processDoc doc
    whenJust mbMetadata $ \metadata -> modify (\s -> s & (buf % #lastMessageContext @(Lens' (MessageBuffer) MessageContext) .~ metadata))
    print r
  SetStyle mbStyle -> setStyle' mbStyle
  ModifyBuffer f -> do
    modify (\s -> s & buf %~ f)
    use buf
  GetBuffer -> use buf

-- | Get the context of the last printed message.
-- Returns the 'MessageContext' which includes information about the rule that generated
-- the message, paragraph state, and formatting requirements.
getLastMessageContext ::
  Print :> es
  => Eff es MessageContext
getLastMessageContext = view #lastMessageContext <$> modifyBuffer id

-- | Print a string (well, Text).
printText ::
  Print :> es
  => Text -- ^ Message.
  -> Eff es ()
printText t = do
  rc <- view #ruleContext <$> modifyBuffer id
  (shouldBreak, newlyModifiedToPrint) <- checkForLinebreaking rc t
  when (newlyModifiedToPrint /= "") $ printDoc (Just (MessageContext rc False shouldBreak False False newlyModifiedToPrint)) . PP.pretty $ newlyModifiedToPrint

runOnParagraph ::
  Print :> es
  => Eff es ()
runOnParagraph = void $ modifyBuffer (\m -> m & #lastMessageContext % #runningOnParagraph .~ True)

-- | Set the message context to "looking" mode.
-- In looking mode, paragraph breaks between different rules are suppressed and single line breaks are forced,
-- creating a more compact, flowing format suitable for room descriptions and object examinations.
-- This is typically called at the beginning of "look" actions and related room description activities.
runOnLookingParagraph ::
  Print :> es
  => Eff es ()
runOnLookingParagraph = void $ modifyBuffer (\m -> m & #lastMessageContext % #runningOnLookingParagraph .~ True)

-- | Determine appropriate line breaks and whitespace for message formatting.
-- Handles paragraph breaks between different rules and manages spacing consistency.
-- Automatically handles edge cases like empty text or text with only whitespace.
checkForLinebreaking ::
  Print :> es
  => Text -- ^ Current rule context
  -> Text -- ^ Message text to process
  -> Eff es (Bool, Text) -- ^ (should add linebreak after, processed text)
checkForLinebreaking rule t
  | T.null t = pure (False, "") -- Empty message, nothing to process
  | T.all (== '\n') t = pure (False, t) -- All whitespace, preserve but no linebreak
  | otherwise = do
  lsc <- getLastMessageContext
  let hangingSpace = T.length $ T.takeWhileEnd (=='\n') (lastPrint lsc)

  -- Determine if we need to add paragraph breaks
  newPrepend <- if shouldPrintPbreak lsc -- Force break if explicitly requested
    || (messageFromRule lsc /= "¬¬¬" -- Not the first message
    && messageFromRule lsc /= rule -- Different rule context
    && not (runningOnParagraph lsc)
    && not (runningOnLookingParagraph lsc))
    then do
        pure "\n\n" -- Add paragraph break
    else
      if runningOnLookingParagraph lsc || shouldPrintLinebreak lsc
      then pure "\n" -- Add single line break
      else pure "" -- No break needed

  -- Ensure consistent spacing (max 2 newlines between elements)
  let newStr1 = newPrepend <> t
      upcomingSpace = T.length $ T.takeWhile (=='\n') newStr1
      newStr = T.dropWhileEnd (=='\n') $ T.drop (max 0 (min upcomingSpace (hangingSpace + upcomingSpace - 2))) newStr1

  -- Determine if linebreak should be added after this message
  let shouldBreakAfter = not (T.null newStr) && T.last newStr `elem` ['.', '!', '?', ':']
  pure (shouldBreakAfter, newStr)

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
  forall s es.
  PartialState s (MessageBuffer) es
  => Maybe MessageAnnotation -- ^ The updated style.
  -> Eff es ()
setStyle' s = buf % (#style @(Lens' (MessageBuffer) (Maybe MessageAnnotation))) .= s

-- | Execute an action with a temporary message style.
-- Sets the specified style for the duration of the action, then restores the previous style.
-- Useful for applying consistent styling to a block of messages.
withStyle ::
  forall es a.
  Print :> es
  => Maybe MessageAnnotation
  -> Eff es a
  -> Eff es a
withStyle s f = do
  b <- getBuffer
  setStyle s
  r <- f
  setStyle (style b)
  return r