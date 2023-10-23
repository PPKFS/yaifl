module Yaifl.Actions.OutOfWorld
  ( superbriefAction
  , briefAction
  , verboseAction
  ) where

import Solitude
import Yaifl.Metadata
import Effectful.Optics
import Yaifl.Text.SayQQ
import Yaifl.Rules.RuleEffects
import Yaifl.Actions.Action

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
  [sayingParagraph|{t} is now in its "superbrief" mode, which always gives short descriptions of locations (even if you haven't been there before).|]

briefAction :: OutOfWorldAction wm
briefAction = OutOfWorldAction "prefer sometimes abbreviated room descriptions" $ do
  t <- changeRoomAbbreviations SometimesAbbreviatedRoomDescriptions
  [sayingParagraph|{t} is now in its "brief" printing mode, which gives long descriptions of places never before visited and short descriptions otherwise.|]

verboseAction :: OutOfWorldAction wm
verboseAction = OutOfWorldAction "prefer never abbreviated room descriptions" $ do
  t <- changeRoomAbbreviations NoAbbreviatedRoomDescriptions
  [sayingParagraph|{t} is now in its "verbose" mode, which always gives long descriptions of locations (even if you've been there before).|]