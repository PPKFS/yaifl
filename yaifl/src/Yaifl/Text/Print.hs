
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Text.Print
  ( -- * Types
    MessageBuffer (..)
  , MessageContext(..)
  , Print(..)
  , Has(..)
  , PartialState
  -- * Smart constructors
  , blankMessageBuffer
  -- * Buffer modification
  , setStyle
  , modifyBuffer
  , runOnParagraph
  , runOnLookingParagraph
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
import qualified Data.Text as T

type StyledDoc = PP.Doc PPTTY.AnsiStyle

data MessageContext = MessageContext
  { messageFromRule :: Text
  , runningOnParagraph :: Bool
  , shouldPrintLinebreak :: Bool
  , runningOnLookingParagraph :: Bool
  , shouldPrintPbreak :: Bool
  , lastPrint :: Text
  } deriving stock (Show, Ord, Generic, Eq)

data Print :: Effect where
  ModifyBuffer :: (MessageBuffer -> MessageBuffer) -> Print m MessageBuffer
  PrintDoc :: Maybe MessageContext -> StyledDoc -> Print m ()
  SetStyle :: Maybe PPTTY.AnsiStyle -> Print m ()

data MessageBuffer = MessageBuffer
  { buffer :: [StyledDoc] -- ^ Current messages held before flushing.
  , lastMessageContext :: MessageContext -- ^ some metadata about the last printed message, to deal with pbreaks and lines
  , style :: Maybe PPTTY.AnsiStyle -- ^ Current formatting; 'Nothing' = plain.
  , context :: [StyledDoc] -- ^ Possibly nested prefixes before every message.
  , ruleContext :: Text -- ^ the currently executing rule
  } deriving stock (Show, Generic)

makeEffect ''Print
makeFieldLabelsNoPrefix ''MessageBuffer
makeFieldLabelsNoPrefix ''MessageContext

blankMessageBuffer :: MessageBuffer
blankMessageBuffer = MessageBuffer [] (MessageContext "¬¬¬" False False False False "") Nothing [] ""

processDoc ::
  forall s es.
  PartialState s MessageBuffer es
  => StyledDoc
  -> Eff es StyledDoc
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

runPrintPure ::
  forall s es a.
  PartialState s MessageBuffer es
  => Eff (Print : es) a
  -> Eff es a
runPrintPure = interpret $ \_ -> \case
  PrintDoc mbMetadata doc -> do
    r <- processDoc doc
    modify (\s -> s & buf % (#buffer @(Lens' MessageBuffer [StyledDoc])) %~ (r:))
    whenJust mbMetadata $ \metadata -> modify (\s -> s & buf % #lastMessageContext @(Lens' MessageBuffer MessageContext) .~ metadata)
  SetStyle mbStyle -> setStyle' mbStyle
  ModifyBuffer f -> do
    modify (\s -> s & buf %~ f)
    use buf

runPrintIO ::
  forall s es a.
  IOE :> es
  => PartialState s MessageBuffer es
  => Eff (Print : es) a
  -> Eff es a
runPrintIO = interpret $ \_ -> \case
  PrintDoc mbMetadata doc -> do
    r <- processDoc doc
    whenJust mbMetadata $ \metadata -> modify (\s -> s & (buf % #lastMessageContext @(Lens' MessageBuffer MessageContext) .~ metadata))
    print r
  SetStyle mbStyle -> setStyle' mbStyle
  ModifyBuffer f -> do
    modify (\s -> s & buf %~ f)
    use buf

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

runOnLookingParagraph ::
  Print :> es
  => Eff es ()
runOnLookingParagraph = void $ modifyBuffer (\m -> m & #lastMessageContext % #runningOnLookingParagraph .~ True)

-- we want to add the possible whitespace /before/ the next thing we print, rather than
-- at the /end/ of the thing that caused it.
-- we need to add a pbreak if:
-- we have finished the rule which the last printed text was from.
-- |
checkForLinebreaking ::
  Print :> es
  => Text
  -> Text
  -> Eff es (Bool, Text)
checkForLinebreaking _rule "" = pure (False, "") -- this is nothing, so we just ignore it
checkForLinebreaking rule t = do
  lsc <- getLastMessageContext
  let hangingSpace = T.length $ T.takeWhileEnd (=='\n') (lastPrint lsc)
  --printDoc Nothing . PP.pretty $ show @Text $ (lastPrint lsc, shouldPrintPbreak lsc, messageFromRule lsc, rule, runningOnParagraph lsc, lastPrint lsc)
  -- if we are in a different rule, then we need to write a paragraph break
  newPrepend <- if shouldPrintPbreak lsc -- we need to force a break anyway
    || (messageFromRule lsc /= "¬¬¬" -- this isn't the first message
    && messageFromRule lsc /= rule -- different rule
    && not (runningOnParagraph lsc)
    && not (runningOnLookingParagraph lsc))
    then do
        --printDoc Nothing . PP.pretty $ ("pbreak" :: Text)
        --printDoc Nothing . PP.pretty $ ("\n\n" :: Text)
        --void $ modifyBuffer (\m -> m & #lastMessageContext % #lastPrint .~ "\n\n" & #lastMessageContext % #shouldPrintPbreak .~ False)
        pure "\n\n"
    else
      if runningOnLookingParagraph lsc || shouldPrintLinebreak lsc then pure "\n"
      else pure ""
  -- we want there to be max two spaces between what we just printed, what we are appending, and what is coming up
  let newStr1 = newPrepend <> t
      upcomingSpace = T.length $ T.takeWhile (=='\n') newStr1
  -- if hanging space + upcoming space is N, and N is greater than 2, we want to drop (N - 2) newlines from the start?
      newStr = T.dropWhileEnd (=='\n') $ T.drop (max 0 (min upcomingSpace (hangingSpace + upcomingSpace - 2))) newStr1
  -- then we want to check if we should be putting a linebreak after this message
  pure (T.empty /= t && T.last t `elem` ['.', '!', '?', ':'], newStr)

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