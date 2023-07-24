module Yaifl.Rules.WhenPlayBegins
  ( whenPlayBeginsName
  , whenPlayBeginsRules
  , introText
  ) where

import Solitude
import Effectful.Optics
import qualified Data.Text as T
import qualified Prettyprinter.Render.Terminal as PPTTY

import Breadcrumbs ( Breadcrumbs, addAnnotation )
import Yaifl.Metadata ( firstRoom, title,  Metadata )
import Yaifl.Model.Objects.Move ( move )
import Yaifl.Model.Objects.Query ( NoMissingObjects, getCurrentPlayer )
import Yaifl.Text.Print ( Print, printText, setStyle )
import Yaifl.Model.Properties.Enclosing ( Enclosing )
import Yaifl.Model.Properties.Has ( WMHasProperty )
import Yaifl.Rules.Rule
import Yaifl.Rules.Rulebook ( Rulebook(..), noRulebookArguments )
import Yaifl.Rules.Run ( failRuleWithError )
import Yaifl.Rules.RuleEffects

whenPlayBeginsName :: Text
whenPlayBeginsName = "when play begins"

-- | The rulebook that runs at the start of the game.
whenPlayBeginsRules ::
  WMHasProperty wm Enclosing
  => Rulebook wm () () Bool
whenPlayBeginsRules = Rulebook
    whenPlayBeginsName
    Nothing
    noRulebookArguments
    [ makeRule' "Display banner" $ sayIntroText >> rulePass
    , makeRule' "Position player in world" positionPlayer
    , makeRule' "Initial room description" initRoomDescription
    ]

sayIntroText ::
  State Metadata :> es
  => Print :> es
  => Eff es ()
sayIntroText = do
  setStyle (Just (PPTTY.color PPTTY.Green <> PPTTY.bold))
  t <- use #title
  printText $ introText t
  setStyle Nothing
  pass

introText ::
  Text
  -> Text
introText w = fold
  [ longBorder <> "\n"
  , shortBorder <> " " <> w <> " " <> shortBorder <> "\n"
  , longBorder <> "\n\n"
  ]
  where
    shortBorder = "-----"
    longBorder = mconcat $ replicate
      (2 * T.length shortBorder + T.length w + 2) "-"

initRoomDescription ::
  Breadcrumbs :> es
  => ActionHandler wm :> es
  => Eff es (Maybe Bool)
initRoomDescription = do
  parseAction (ActionOptions True Nothing) "look" >>= (\case
     Left txt -> addAnnotation txt
     Right True -> pass
     Right False -> error "Could not find the looking action.")
  rulePass

positionPlayer ::
  NoMissingObjects wm es
  => WMHasProperty wm Enclosing
  => Eff es (Maybe Bool)
positionPlayer = do
  fr <- use #firstRoom
  pl <- getCurrentPlayer
  m <- move pl fr
  if m then return Nothing else failRuleWithError "Failed to move the player."