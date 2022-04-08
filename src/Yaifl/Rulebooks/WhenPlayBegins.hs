{-|
Module      : Yaifl.Rulebooks.WhenPlayBegins
Description : The rulebook to be run at the start of the game.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

module Yaifl.Rulebooks.WhenPlayBegins 
  ( whenPlayBeginsName
  , whenPlayBeginsRules
  ) where
import Solitude
import Yaifl.Properties.Enclosing
import Yaifl.Rulebooks.Rulebook
import Yaifl.Properties.Property

whenPlayBeginsName :: Text
whenPlayBeginsName = "when play begins"

-- | The rulebook that runs at the start of the game.
whenPlayBeginsRules
  :: WMHasProperty wm Enclosing
  => Rulebook wm () () Bool
whenPlayBeginsRules = Rulebook
    whenPlayBeginsName
    Nothing
    (ParseArguments (const $ return (Just ())) )
    [ makeRule' "Display banner" $ sayIntroText >> rulePass
    , makeRule' "Position player in world" positionPlayer
    , makeRule' "Initial room description" initRoomDescription
    ]
