{-# LANGUAGE RecordWildCards #-}
module Main where

import Yaifl.Prelude

import Yaifl.Run
import Yaifl.Zork.World.House
import Yaifl.Zork.World

main :: IO ()
main = do
  r <- gameHarness "Zork" (defaultZorkOptions) zorkWorld testMeWith
  mapM_ putTextLn (lines r)
