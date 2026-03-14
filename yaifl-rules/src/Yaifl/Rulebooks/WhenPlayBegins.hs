module Yaifl.Rulebooks.WhenPlayBegins
  ( whenPlayBeginsRules
  ) where

import Yaifl.Prelude
import qualified Data.Text as T

import Breadcrumbs ( addAnnotation )
import Yaifl.Move ( move )
import Yaifl.Actions.Args
import Yaifl.Effects.ObjectQuery
import Yaifl.Property.Has ( WMWithProperty )
import Yaifl.Enclosing.Kind ( Enclosing )
import Yaifl.Metadata ( Metadata )
import Yaifl.ObjectLike
import Yaifl.Rulebook
import Yaifl.Effects.RuleEffects
import Yaifl.Effects.Print
import Yaifl.Room.Kind
import Yaifl.Person.Query
import Yaifl.Thing.Query

whenPlayBeginsName :: Text
whenPlayBeginsName = "when play begins"

-- | The rulebook that runs at the start of the game.
whenPlayBeginsRules ::
  WMWithProperty wm Enclosing
  => Rulebook wm Unconstrained () Bool
whenPlayBeginsRules = Rulebook
  { name = whenPlayBeginsName
  , defaultOutcome = Nothing
  , rules =
      [ makeRule' "Display banner" $ sayIntroText >> rulePass
      , makeRule' "Position player in world" positionPlayer
      , makeRule' "Initial room description" initRoomDescription
      ]
  }

sayIntroText ::
  State Metadata :> es
  => Print  :> es
  => Eff es ()
sayIntroText = do
  setStyle (Just (colour (Colour 0xff147421) <> bold))
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
  RuleEffects wm es
  => Eff es (Maybe Bool)
initRoomDescription = do
  parseAction (ActionOptions { silently = True, hidePrompt = True }) [] "look" >>= (\case
    Left txt -> addAnnotation txt
    Right True -> pass
    Right False -> error "Could not find the looking action.")
  rulePass

positionPlayer ::
  WithoutMissingObjects wm es
  => WMWithProperty wm Enclosing
  => Eff es (Maybe Bool)
positionPlayer = do
  fre <- use #firstRoom
  fr <- getRoom fre
  pl <- getPlayer'
  plLoc <- getLocation pl
  when (isVoid plLoc) $ void $ move pl fr
  return Nothing