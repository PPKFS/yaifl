module Yaifl.Say 
(
    say,
    sayLn,
    sayDbg,
    sayDbgLn,
    indentDbg,
    setStyle,
    MessageBuffer(..),
    HasMessageBuffer,
    messageBuffer
) where

import Relude
import Lens.Micro.Platform
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
        _msgStyle :: Maybe PPTTY.AnsiStyle
    }
makeClassy ''MessageBuffer

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