module EffectStack where

import Yaifl.Prelude hiding ( Reader, runReader )

import Breadcrumbs
import Yaifl.Std.Parser
import Yaifl.Direction.Kind
import Yaifl.WorldModel
import Yaifl.Effects.Print
import Yaifl.Std.World
import Yaifl.Actions.Collection
import Effectful.Error.Static (runError)

import Yaifl.Visibility
import Effectful.Provider.List (type (++))
import Yaifl.Effects.Interpreters
import Gui

convertStack ::
  forall wm es' a.
  IOE :> es'
  => State GuiState :> es'
  => (Ord (WMDirection wm), Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
  => HasLookingProperties wm
  => World wm
  -> ActionCollection wm
  -> Eff (EffStack wm ++ es') a
  -> Eff es' (a, World wm)
convertStack w ac =
  fmap (either (error . show) id)
  . inject
  . runError
  . runBreadcrumbs Nothing
  . runStateShared w
  . runPrintPure
  . zoomState #actions
  . zoomState @(World wm) #metadata
  . runQueryAsLookup
  . runTraverseAsLookup
  . evalStateShared ac
  . runInputFromGUI
  . zoomState @(World wm) #activities
  . zoomState @(World wm) #responses
  . zoomState @(World wm) #adaptiveNarrative
  . runActionHandlerAsWorldActions
