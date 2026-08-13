module Yaifl.Rulebooks.WhenPlayBegins
  ( whenPlayBeginsRules
  ) where

import Yaifl.Prelude

import Breadcrumbs ( addAnnotation )
import Yaifl.Move ( move )
import Yaifl.Actions.Args
import Yaifl.Effects.ObjectQuery
import Yaifl.Property.Has ( WMWithProperty )
import Yaifl.Enclosing.Kind ( Enclosing )
import Yaifl.ObjectLike
import Yaifl.Rulebook
import Yaifl.Effects.RuleEffects
import Yaifl.Room.Kind
import Yaifl.Person.Query
import Yaifl.Thing.Query
import Yaifl.Activity
import Yaifl.Activities.PrintingTheBannerText (WithPrintingTheBannerText)

whenPlayBeginsName :: Text
whenPlayBeginsName = "when play begins"

-- | The rulebook that runs at the start of the game.
whenPlayBeginsRules ::
  WithPrintingTheBannerText wm
  => WMWithProperty wm Enclosing
  => Rulebook wm Unconstrained () Bool
whenPlayBeginsRules = Rulebook
  { name = whenPlayBeginsName
  , defaultOutcome = Nothing
  , rules =
      [ makeRule' "Display banner" $ doActivity #printingTheBannerText () >> rulePass
      , makeRule' "Position player in world" positionPlayer
      , makeRule' "Initial room description" initRoomDescription
      ]
  }


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