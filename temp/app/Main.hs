{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Main (main) where

import Yaifl
import Relude

data Bar = F deriving Show
main :: IO ()
main = putStrLn ("hi" :: String)