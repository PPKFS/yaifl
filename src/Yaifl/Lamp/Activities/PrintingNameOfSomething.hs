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
, printNameDefiniteUncapitalised
, capitalThe
, printingNameOfSomethingImpl
, SayOptions(..)
, Article(..)
, Capitalisation(..)
) where

import Solitude

import Breadcrumbs ( Breadcrumbs )
import Yaifl.Core.Actions.Activity ( Activity, makeActivity, doActivity, ActivityCollection )
import Yaifl.Core.AdaptiveText.Eval ( sayAdaptive )
import Yaifl.Core.Object ( Object(..), AnyObject )
import Yaifl.Core.Objects.Query ( ObjectTraverse, ObjectLike, NoMissingObjects, getObject )
import Yaifl.Core.Rulebooks.Rule ( ActionHandler, makeRule )
import Yaifl.Core.Say ( Saying, say )
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
  => Saying :> es
  => ActionHandler wm :> es
  => Breadcrumbs :> es
  => ObjectTraverse wm :> es
  => State (ActivityCollection wm) :> es
  => ObjectLike wm o
  => o
  -> Eff es ()
printName = printNameEx noSayOptions

printNameDefiniteUncapitalised ::
  NoMissingObjects wm es
  => Saying :> es
  => ActionHandler wm :> es
  => Breadcrumbs :> es
  => State (ActivityCollection wm) :> es
  => ObjectTraverse wm :> es
  => ObjectLike wm o
  => o
  -> Eff es ()
printNameDefiniteUncapitalised = printNameEx (SayOptions Definite Uncapitalised)

printNameEx ::
  NoMissingObjects wm es
  => Saying :> es
  => ActionHandler wm :> es
  => State (ActivityCollection wm) :> es
  => ObjectTraverse wm :> es
  => Breadcrumbs :> es
  => ObjectLike wm o
  => SayOptions
  -> o
  -> Eff es ()
printNameEx p o = do
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
    (makeRule "" (\o -> sayAdaptive (_objName o) o >> return (Just ())))