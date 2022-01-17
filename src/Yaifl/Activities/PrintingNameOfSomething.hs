module Yaifl.Activities.PrintingNameOfSomething
( printNameEx
, printName
, capitalThe
, printingNameOfSomethingImpl
, SayOptions(..)
, Article(..)
, Capitalisation(..)
) where

import Yaifl.Activities.Common
import Yaifl.Common
import Yaifl.Prelude
import Yaifl.Rulebooks
import Yaifl.Messages
import Yaifl.ObjectLookup


data SayOptions = NoOptions | SayOptions Article Capitalisation

data Article = Indefinite | Definite

data Capitalisation = Capitalised | Uncapitalised

noSayOptions :: SayOptions
noSayOptions = NoOptions

capitalThe :: SayOptions
capitalThe = SayOptions Definite Capitalised

printName
  :: ObjectLike s o
  => o
  -> State (World s) ()
printName o = printNameEx o noSayOptions

printNameEx
  :: ObjectLike s o
  => o
  -> SayOptions
  -> State (World s) ()
printNameEx o p = do
  e <- getObject o
  let pr = doActivity printingNameOfSomething <$> e
  whenJust pr (\p' -> do
    case p of
        NoOptions -> p'
        SayOptions Indefinite Capitalised -> do say "A "; p'
        SayOptions Definite Capitalised -> do say "The "; p'
        SayOptions Indefinite Uncapitalised -> do say "a "; p'
        SayOptions Definite Uncapitalised -> do say "the "; p'
    pass )
  pass

printingNameOfSomethingImpl :: Activity s (AnyObject s) ()
printingNameOfSomethingImpl = makeActivity "Printing the name of something"
    (makeRule "" (\o w -> (Just (), say' (_objName o) w)))