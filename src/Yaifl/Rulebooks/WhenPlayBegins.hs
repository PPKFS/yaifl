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
  , addWhenPlayBegins
  , introText
  ) where
import Solitude
import Yaifl.Properties.Enclosing
import Yaifl.Rulebooks.Rulebook
import Yaifl.Properties.Property
import Yaifl.World
import qualified Prettyprinter.Render.Terminal as PPTTY
import qualified Data.Text as T
import Yaifl.Objects.Missing
import Yaifl.Say
import Yaifl.Rulebooks.Args
import Yaifl.Objects.Move
import Yaifl.Actions.Action

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

sayIntroText
  :: MonadWorld s m
  => m ()
sayIntroText = do
  setSayStyle (Just (PPTTY.color PPTTY.Green <> PPTTY.bold))
  t <- gets _title
  say $ introText t
  setSayStyle Nothing
  pass

introText
  :: Text
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

initRoomDescription :: 
  NoMissingObjects m
  => MonadWorld wm m
  => m (Maybe a)
initRoomDescription = do
  ua <- playerNoArgs
  tryAction "looking" ua >> rulePass

  
positionPlayer :: 
  MonadWorld wm m
  => WMHasProperty wm Enclosing
  => m (Maybe Bool)
positionPlayer = do
  fr <- gets _firstRoom
  pl <- gets _currentPlayer
  case fr of
    Nothing -> failRuleWithError
      "No rooms have been made, so cannot place the player."
    Just fr' -> do
      m <- move pl fr'
      if m then return Nothing else failRuleWithError "Failed to move the player."

addWhenPlayBegins ::
  MonadWorld wm m
  => Rule wm () Bool
  -> m ()
addWhenPlayBegins r = whenPlayBegins %= addRuleLast r