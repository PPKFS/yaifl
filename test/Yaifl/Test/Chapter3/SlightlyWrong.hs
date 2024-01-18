module Yaifl.Test.Chapter3.SlightlyWrong where

import Yaifl
import Yaifl.Model.Metadata
import Yaifl.Model.Kinds.Object
import Yaifl.Game.Create.Object
import Yaifl.Game.Create.RoomConnection
import Yaifl.Test.Common
import Solitude
import Yaifl.Model.Kinds.Room
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Model.Rules.Rulebook
import Yaifl.Text.SayQQ
import Yaifl.Model.Rules.RuleEffects
import Yaifl.Text.DynamicText

ex4 :: (Text, [Text], Game PlainWorldModel ())
ex4 = ("Slightly Wrong", ex4TestMeWith, ex4World)

awnN :: DynamicText wm
awnN = "Awning"

awnDesc :: DynamicText wm
awnDesc =
  [wrappedText|A tan awning is stretched on tent poles over the dig-site, providing a little shade to the workers here; you are at the bottom of a square
  twenty feet on a side, marked out with pegs and lines of string. Uncovered in the south face of this square is an awkward opening into the earth.|]

swcN :: DynamicText wm
swcN = "Slightly Wrong Chamber"

swcDesc :: DynamicText wm
swcDesc = DynamicText $ Right ("description of slightly wrong chamber", RuleLimitedEffect $ do
  obj <- view #objectData <$> getMentionedRoom
  when (isVisited obj /= Visited)
    [sayingTell|When you first step into the room, you are bothered by the sense that something is not quite right: perhaps the lighting, perhaps the angle of the walls. |]
  [sayingTell|A mural on the far wall depicts a woman with a staff, tipped with a pine-cone. She appears to be watching you.|]
  )

ex4World :: Game PlainWorldModel ()
ex4World = do
    setTitle "Slightly Wrong"
    swc <- addRoom' swcN swcDesc pass
    awn <- addRoom' awnN awnDesc pass

    swc `isSouthOf` awn
    pass

ex4TestMeWith :: [Text]
ex4TestMeWith = ["north", "south"]