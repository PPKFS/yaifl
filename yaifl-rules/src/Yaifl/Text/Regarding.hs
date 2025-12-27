module Yaifl.Text.Regarding where

import Yaifl.Prelude

import Yaifl.Effects.ObjectQuery
import Yaifl.AnyObject
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Person.Query

regarding ::
  State (AdaptiveNarrative wm) :> es
  => CanBeAny wm a
  => Maybe a
  -> Eff es ()
regarding mbObj = do
  #priorNamedObject .= (toAny <$> mbObj)
  #priorQuantity .= 1

regardingMany ::
  State (AdaptiveNarrative wm) :> es
  => Eff es ()
regardingMany = do
  #priorQuantity .= 2

regardingNothing ::
  State (AdaptiveNarrative wm) :> es
  => Eff es ()
regardingNothing = regarding (Nothing :: Maybe (AnyObject wm))

regardingThePlayer ::
  forall wm es.
  State (AdaptiveNarrative wm) :> es
  => WithoutMissingObjects wm es
  => Eff es ()
regardingThePlayer = do
  p <- getPlayer' @wm
  regarding $ Just $ toAny p
