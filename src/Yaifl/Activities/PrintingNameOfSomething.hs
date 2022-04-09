{-|
Module      : Yaifl.Actions.Action
Description : An action is a verb that is carried out by the player (or an NPC).
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

module Yaifl.Activities.PrintingNameOfSomething
( printNameEx
, printName
, capitalThe
, printingNameOfSomethingImpl
, SayOptions(..)
, Article(..)
, Capitalisation(..)
) where

import Yaifl.Activities.Activity
import Yaifl.Rulebooks.Rulebook
import Yaifl.Say
import Yaifl.Objects.Missing
import Yaifl.Objects.Object
import Yaifl.WorldInfo
import Yaifl.Objects.Query
import Solitude



data SayOptions = NoOptions | SayOptions Article Capitalisation

data Article = Indefinite | Definite

data Capitalisation = Capitalised | Uncapitalised

noSayOptions :: SayOptions
noSayOptions = NoOptions

capitalThe :: SayOptions
capitalThe = SayOptions Definite Capitalised

printName
  :: NoMissingObjects m
  => MonadWorld s m
  => ObjectLike s o
  => o
  -> m ()
printName o = printNameEx o noSayOptions

printNameEx
  :: NoMissingObjects m
  => MonadWorld s m
  => ObjectLike s o
  => o
  -> SayOptions
  -> m ()
printNameEx o p = do
  e <- getObject o
  let pr = doActivity printingNameOfSomething e
  case p of
    NoOptions -> pr
    SayOptions Indefinite Capitalised -> do say "A "; pr
    SayOptions Definite Capitalised -> do say "The "; pr
    SayOptions Indefinite Uncapitalised -> do say "a "; pr
    SayOptions Definite Uncapitalised -> do say "the "; pr
  pass

printingNameOfSomethingImpl :: Activity s (AnyObject s) ()
printingNameOfSomethingImpl = makeActivity "Printing the name of something"
    (makeRule "" (\o -> say (_objName o) >> return (Just ())))