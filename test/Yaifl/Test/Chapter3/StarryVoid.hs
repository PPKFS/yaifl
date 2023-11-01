module Yaifl.Test.Chapter3.StarryVoid where


import Yaifl
import Yaifl.Metadata
import Yaifl.Model.Objects.Create
import Solitude
import Yaifl.Test.Common
import Yaifl.Model.Objects.RoomConnections
import Yaifl.Text.Say
import Yaifl.Text.SayQQ
import Yaifl.Rules.Adding
import Yaifl.Actions.Going
import Yaifl.Rules.Rule
import Yaifl.Text.DynamicText
import Yaifl.Text.AdaptiveNarrative

boothDesc :: DynamicText wm
boothDesc = DynamicText $ Right ("description of magician's booth door", RuleLimitedEffect $ withRoom $ \r -> do
  pass

  )


starryVoid :: Game PlainWorldModel ()
starryVoid = do
  setTitle "Starry Void"
  tcr <- addRoom "The Centre Ring" ""
  tmb <- addDoor "The magician's booth" boothDesc
  pass