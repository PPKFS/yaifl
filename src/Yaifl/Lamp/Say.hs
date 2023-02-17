{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Yaifl.Lamp.Say where

import Solitude
import Yaifl.Core.Rulebooks.Rule
import Yaifl.Core.Object
import Yaifl.Core.Print
import Yaifl.Core.WorldModel
import Effectful.Optics (use)
import GHC.TypeLits
import Yaifl.Core.Actions.Activity
import Yaifl.Core.Objects.Query
import Yaifl.Core.AdaptiveNarrative
import Data.Text.Display


--instance SayableValue wm ("our" :: Symbol) where

instance SayableValue a wm => SayableValue (Maybe a) wm where
  sayText s = fromMaybe "" <$> traverse sayText s
  say s = whenJust s say

instance WithPrintingNameOfSomething wm => SayableValue (Room wm) wm where
  say = printName
  sayText o = sayText $ o ^. #name

instance WithPrintingNameOfSomething wm => SayableValue (Thing wm) wm where
  say = printName
  sayText o = sayText $ o ^. #name

instance WithPrintingNameOfSomething wm => SayableValue (AnyObject wm) wm where
  say = printName
  sayText o = sayText $ o ^. #name

type WithResponse wm (name :: Symbol) = LabelOptic' name A_Lens (WMResponses wm) (Response wm)

sayResponse ::
  forall wm es.
  RuleEffects wm es
  => Lens' (WMResponses wm) (Response wm)
  -> Eff es ()
sayResponse aL = do
  join $ use @(ResponseCollector wm) $ #responseCollection % aL % #runResponse

type WithPrintingNameOfSomething wm = (Display (WMSayable wm), SayableValue (WMSayable wm) wm, WithActivity "printingNameOfSomething" wm (AnyObject wm) ())

printName ::
  NoMissingObjects wm es
  => Print :> es
  => ActionHandler wm :> es
  => ObjectTraverse wm :> es
  => State (ActivityCollector wm) :> es
  => State (ResponseCollector wm) :> es
  => State (AdaptiveNarrative wm) :> es
  => WithPrintingNameOfSomething wm
  => ObjectLike wm o
  => o
  -> Eff es ()
printName o = do
  e <- getObject o
  void $ doActivity #printingNameOfSomething e

printingNameOfSomethingImpl :: Activity s (AnyObject s) ()
printingNameOfSomethingImpl = makeActivity "Printing the name of something"
    [makeRule "" (\o -> do
      t <- sayText $ o ^. #name
      printText t >> return (Just ())) ]