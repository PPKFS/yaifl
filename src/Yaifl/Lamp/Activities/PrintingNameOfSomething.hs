{-|
Module      : Yaifl.Actions.Action
Description : An action is a verb that is carried out by the player (or an NPC).
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

module Yaifl.Lamp.Activities.PrintingNameOfSomething
( printNameEx
, printName
, capitalThe
, printingNameOfSomethingImpl
, SayOptions(..)
, Article(..)
, Capitalisation(..)
) where

import Yaifl.Core.Actions.Activity ( Activity, makeActivity, doActivity, ActivityCollection )
import Yaifl.Core.Say
import Yaifl.Core.Objects.Object
import Yaifl.Core.Objects.Query
import Yaifl.Core.Rulebooks.Rule
import Yaifl.Core.Logger
import Yaifl.Core.Common
import Cleff.State
import qualified Yaifl.Core.Actions.Activity as Ac

data SayOptions = NoOptions | SayOptions Article Capitalisation

data Article = Indefinite | Definite

data Capitalisation = Capitalised | Uncapitalised

noSayOptions :: SayOptions
noSayOptions = NoOptions

capitalThe :: SayOptions
capitalThe = SayOptions Definite Capitalised

printName :: 
  NoMissingObjects wm es
  => Log :> es
  => Saying :> es
  => ActionHandler :> es
  => ObjectTraverse wm :> es
  => State (ActivityCollection wm) :> es
  => ObjectLike wm o
  => o
  -> Eff es ()
printName o = printNameEx o noSayOptions

printNameEx :: 
  NoMissingObjects wm es
  => Log :> es
  => Saying :> es
  => ActionHandler :> es
  => State (ActivityCollection wm) :> es
  => ObjectTraverse wm :> es
  => ObjectLike wm o
  => o
  -> SayOptions
  -> Eff es ()
printNameEx o p = do
  e <- getObject o
  let pr = doActivity Ac.printingNameOfSomething e
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