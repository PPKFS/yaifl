module Main where

import BearMonadTerminal
import BearLibTerminal.Raw
import Control.Monad
import Control.Exception
import Data.Text ( Text )
import qualified Data.Text as T
import Omni.Speed

main :: IO ()
main = do
  bracket_
    (void terminalOpen >> resetTerminal)
    (void terminalClose)
    runLoop

runLoop :: IO ()
runLoop = do
  terminalClear
  printEntries
  putStrLn "hi"
  void $ terminalPrintText 2 23 "[color=orange]ESC.[/color] Exit"
  void $ terminalPrintExtText 77 22 0 0 alignRight "library version 0.1.0.0"
  void $ terminalPrintExtText 77 23 0 0 alignRight "bearmonadterminal (Hello from Haskell!)"
  terminalRefresh
  c <- terminalReadCode
  case c of
    224 -> return ()
    41 -> testSpeed
    x -> do
      print x
      runLoop

entries :: [(Text, IO ())]
entries =
  [ ("Basic output", return ())
  , ("Default font", return ())
  , ("Tilesets", return ())
  , ("Sprites", return ())
  , ("Manual cellsize", return ())
  , ("Auto-generated tileset", return ())
  , ("Multiple fonts", return ())
  , ("Text alignment", return ())
  , ("Formatted log", return ())
  , ("Layers", return ())
  , ("Extended 1: basics", return ())
  , ("Extended 2: smooth scroll", return ())
  , ("Dynamic sprites", return ())
  , ("Speed", return ())
  , ("Input 1: keyboard", return ())
  , ("Input 2: mouse", return ())
  , ("Input 3: text input", return ())
  , ("Input 4: filtering", return ())
  , ("Window resizing", return ())
  , ("Examining cell contents", return ())
  ]

printEntry :: (Int, (Text, IO ())) -> IO ()
printEntry (i, (n, ac)) = do
  let shortcut = toEnum $ if i < 9 then fromEnum '1' + i else fromEnum 'a' + (i-9)
  void $ terminalPrintText 2 (1+i) (mconcat ["[color=orange]", T.singleton shortcut, ".[/color][color=gray]", n])

printEntries :: IO ()
printEntries = mapM_ printEntry (zip [0..] entries)

alignRight :: Int
alignRight = 2

resetTerminal :: IO ()
resetTerminal = do
  -- todo: font:default, input filter to keyboard
  void $ terminalSet defaultWindowOptions { title = Just "Omni: menu" }
  terminalColorNameText "white"
