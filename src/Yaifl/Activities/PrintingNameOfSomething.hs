module Yaifl.Activities.PrintingNameOfSomething
( printNameEx
, printName
, capitalThe
, printingNameOfSomethingImpl
, SayOptions(..)
, Article(..)
, Capitalisation(..)
) where
TODO: extract
import Yaifl.Activities.Common
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
  :: NoMissingObjects s m
  => MonadWorld s m
  => ObjectLike s o
  => o
  -> m ()
printName o = printNameEx o noSayOptions

printNameEx
  :: NoMissingObjects s m
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