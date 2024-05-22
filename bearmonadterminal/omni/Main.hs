module Main where

import BearMonadTerminal
import BearLibTerminal.Raw
import Control.Monad

main :: IO ()
main = do
  void terminalOpen
  resetTerminal
  runLoop

runLoop :: IO ()
runLoop = do
  terminalClear
  printEntries
  void $ terminalPrintText 2 23 "[color=orange]ESC.[/color] Exit"
  void $ terminalPrintExtText 77 22 0 0 alignRight "library version 0.1.0.0"
  void $ terminalPrintExtText 77 23 0 0 alignRight "http://wyrd.name/en:bearlibterminal"
  terminalRefresh

printEntries :: IO ()
printEntries = return ()

alignRight :: Int
alignRight = -1

resetTerminal :: IO ()
resetTerminal = do
  -- todo: font:default, input filter to keyboard
  void $ terminalSet defaultWindowOptions { title = Just "Omni: menu" }
  terminalColorNameText "white"
