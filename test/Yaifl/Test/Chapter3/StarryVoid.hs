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
boothDesc = DynamicText $ Right ("description of magician's booth door", RuleLimitedEffect $ do
  obj <- view #objectData <$> getMentionedRoom
  error ""
 {- case_
    [ (playerLocatedIn obj)

    ]
  when (isVisited obj /= Visited)
    [sayingTell|When you first step into the room, you are bothered by the sense that something is not quite right: perhaps the lighting, perhaps the angle of the walls. |]
  [sayingTell|A mural on the far wall depicts a woman with a staff, tipped with a pine-cone. She appears to be watching you.|] -}
  )


starryVoid :: Game PlainWorldModel ()
starryVoid = do
  setTitle "Starry Void"
  tcr <- addRoom "The Centre Ring" ""
  tmb <- addDoor "The magician's booth" boothDesc
  pass