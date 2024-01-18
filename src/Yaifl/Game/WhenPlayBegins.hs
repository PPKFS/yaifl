module Yaifl.Game.WhenPlayBegins
  ( whenPlayBeginsName
  , whenPlayBeginsRules
  , introText
  ) where

import Solitude
import Effectful.Optics
import qualified Data.Text as T
import qualified Prettyprinter.Render.Terminal as PPTTY

import Breadcrumbs ( Breadcrumbs, addAnnotation )
import Yaifl.Model.Metadata ( Metadata )
import Yaifl.Game.Move ( move )
import Yaifl.Model.Query ( getCurrentPlayer )
import Yaifl.Text.Print ( Print, setStyle, printText )
import Yaifl.Model.Kinds.Enclosing ( Enclosing )
import Yaifl.Model.HasProperty ( WMWithProperty )
import Yaifl.Model.Rules.Rulebook
import Yaifl.Model.Rules.Run ( failRuleWithError )
import Yaifl.Model.Rules.RuleEffects
import Yaifl.Model.Effects
import Yaifl.Model.Actions.Args
import Yaifl.Model.ObjectLike

whenPlayBeginsName :: Text
whenPlayBeginsName = "when play begins"

-- | The rulebook that runs at the start of the game.
whenPlayBeginsRules ::
  WMWithProperty wm Enclosing
  => Rulebook wm Unconstrained () Bool
whenPlayBeginsRules = Rulebook
    whenPlayBeginsName
    Nothing
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
  , longBorder
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
  parseAction (ActionOptions True True) [NoParameter] "look" >>= (\case
     Left txt -> addAnnotation txt
     Right True -> pass
     Right False -> error "Could not find the looking action.")
  rulePass

positionPlayer ::
  NoMissingObjects wm es
  => WMWithProperty wm Enclosing
  => Eff es (Maybe Bool)
positionPlayer = do
  fre <- use #firstRoom
  fr <- getRoom fre
  pl <- getCurrentPlayer
  m <- move pl fr
  if m then return Nothing else failRuleWithError "Failed to move the player."