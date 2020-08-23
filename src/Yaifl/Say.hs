module Yaifl.Say 
(
    say,
    sayLn,
    sayDbg,
    sayDbgLn,
    sayIf,
    sayIfDbg,
    sayIfNothingDbg,
    indentDbg,
    setStyle,
    MessageBuffer(..),
    HasMessageBuffer,
    messageBuffer,
    stdBuffer,
    printMessageBuffer,
    blankMessageBuffer
) where

import Relude
import Control.Lens
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY

data Capitalisation = Capitalised | Uncapitalised
data Definiteness = Indefinite | Definite
data NameStyle = NameStyle Capitalisation Definiteness

type StyledDoc = PP.Doc PPTTY.AnsiStyle
data MessageBuffer = MessageBuffer
    {
        _indentLvl :: Int,
        _stdBuffer :: [StyledDoc],
        _dbgBuffer :: [StyledDoc], -- this one is sent to stderr
        _msgStyle :: Maybe PPTTY.AnsiStyle,
        _isDbg :: Bool
    } deriving Show
makeClassy ''MessageBuffer

blankMessageBuffer :: MessageBuffer
blankMessageBuffer = MessageBuffer 0 [] [] Nothing True
instance HasMessageBuffer w => HasMessageBuffer (w, a) where
    messageBuffer = _1 . messageBuffer
    
defaultStyle :: NameStyle
defaultStyle = NameStyle Capitalised Definite

sayInternal :: HasMessageBuffer x => StyledDoc -> Lens' MessageBuffer [StyledDoc] -> State x ()
sayInternal a buf = do
    w <- get
    let style = w ^. messageBuffer . msgStyle
    messageBuffer . buf %= (:) (maybe id PP.annotate style a)

say :: HasMessageBuffer x => Text -> State x ()
say a = sayInternal (PP.pretty a) stdBuffer

sayLn :: HasMessageBuffer x => Text -> State x ()
sayLn a = say (a <> "\n")

sayIf :: HasMessageBuffer x => Bool -> Text -> State x ()
sayIf iff a = when iff (say a)

sayIfDbg :: HasMessageBuffer x => Bool -> Text -> State x ()
sayIfDbg iff a = when iff (sayDbg a)

sayIfNothingDbg :: HasMessageBuffer x => Maybe a -> Text -> State x ()
sayIfNothingDbg Nothing a = sayDbg a
sayIfNothingDbg (Just _) _ = pass

sayDbg :: HasMessageBuffer x => Text -> State x ()
sayDbg a = do
    w <- get
    let msg = PP.pretty $ replicate (w ^. messageBuffer . indentLvl) ' ' <> toString a
    sayInternal msg dbgBuffer

sayDbgLn :: HasMessageBuffer x => Text -> State x ()
sayDbgLn a = sayDbg (a <> "\n")

indentDbg :: HasMessageBuffer x => Bool -> State x ()
indentDbg b = messageBuffer . indentLvl %= (+) ((if b then 1 else (-1)) * 4)

setStyle :: HasMessageBuffer x => Maybe PPTTY.AnsiStyle -> State x ()
setStyle s = messageBuffer . msgStyle .= s

printMessageBuffer :: HasMessageBuffer x => StateT x IO Text
printMessageBuffer = do
    w <- get
    let rev = reverse (w ^. messageBuffer . stdBuffer)
    when (w ^. messageBuffer . isDbg) $ liftIO $ PPTTY.putDoc $ PP.fillCat $ reverse (w ^. messageBuffer . dbgBuffer)
    liftIO $ PPTTY.putDoc $ PP.fillCat rev
    return $ foldr (\p v -> show p <> v) "" rev