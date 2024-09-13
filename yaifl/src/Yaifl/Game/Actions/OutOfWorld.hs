module Yaifl.Game.Actions.OutOfWorld
  ( superbriefAction
  , briefAction
  , verboseAction
  ) where

import Yaifl.Prelude
import Yaifl.Model.Metadata
import Yaifl.Text.SayQQ
import Yaifl.Model.Action
import Yaifl.Text.Say

changeRoomAbbreviations ::
  State Metadata :> es
  => RoomDescriptions
  -> Eff es Text
changeRoomAbbreviations rd = do
  #roomDescriptions .= rd
  use #title

superbriefAction :: OutOfWorldAction wm
superbriefAction = OutOfWorldAction "prefer always abbreviated room descriptions" $ do
  t <- changeRoomAbbreviations AbbreviatedRoomDescriptions
  [saying|{t} is now in its "superbrief" mode, which always gives short descriptions of locations (even if you haven't been there before).|]

briefAction :: OutOfWorldAction wm
briefAction = OutOfWorldAction "prefer sometimes abbreviated room descriptions" $ do
  t <- changeRoomAbbreviations SometimesAbbreviatedRoomDescriptions
  [saying|{t} is now in its "brief" printing mode, which gives long descriptions of places never before visited and short descriptions otherwise.|]

verboseAction :: OutOfWorldAction wm
verboseAction = OutOfWorldAction "prefer never abbreviated room descriptions" $ do
  t <- changeRoomAbbreviations NoAbbreviatedRoomDescriptions
  [saying|{t} is now in its "verbose" mode, which always gives long descriptions of locations (even if you've been there before).|]