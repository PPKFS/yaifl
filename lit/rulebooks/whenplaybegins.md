# When Play Begins

```haskell file=src/Yaifl/Core/Rulebooks/WhenPlayBegins.hs

module Yaifl.Core.Rulebooks.WhenPlayBegins 
  ( whenPlayBeginsName
  , whenPlayBeginsRules
  , introText
  ) where

import Solitude
import Yaifl.Core.Properties.Enclosing
import Yaifl.Core.Rulebooks.Rulebook
import Yaifl.Core.Properties.Property
import qualified Prettyprinter.Render.Terminal as PPTTY
import qualified Data.Text as T
import Yaifl.Core.Say
import Yaifl.Core.Objects.Move
import Yaifl.Core.Objects.Query
import Yaifl.Core.Rulebooks.Rule
import Yaifl.Core.Common
import Yaifl.Core.Rulebooks.Run
import Yaifl.Core.Logger
import Cleff.State

whenPlayBeginsName :: Text
whenPlayBeginsName = "when play begins"

-- | The rulebook that runs at the start of the game.
whenPlayBeginsRules :: 
  WMHasProperty wm Enclosing
  => Rulebook wm () () Bool
whenPlayBeginsRules = Rulebook
    whenPlayBeginsName
    Nothing
    (ParseArguments (const $ return (Just ())) )
    [ makeRule' "Display banner" $ sayIntroText >> rulePass
    , makeRule' "Position player in world" positionPlayer
    , makeRule' "Initial room description" initRoomDescription
    ]

sayIntroText :: 
  '[State (Metadata wm), Saying] :>> es
  => Eff es ()
sayIntroText = do
  setStyle (Just (PPTTY.color PPTTY.Green <> PPTTY.bold))
  t <- use title
  say $ introText t
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
    shortBorder = "------"
    longBorder = mconcat $ replicate
      (2 * T.length shortBorder + T.length w + 2) "-"

initRoomDescription :: Eff es (Maybe a)
initRoomDescription = do
  parseAction "looking" >> rulePass

parseAction :: t0 -> Eff es a0
parseAction = error ""

positionPlayer :: 
  Log :> es
  => NoMissingObjects wm es
  => WMHasProperty wm Enclosing
  => Eff es (Maybe Bool)
positionPlayer = do
  fr <- use firstRoom
  pl <- use currentPlayer
  m <- move pl fr
  if m then return Nothing else failRuleWithError "Failed to move the player."

```
