module Yaifl.Test.Chapter3.SlightlyWrong where

import Yaifl
import Yaifl.Core.Metadata
import Yaifl.Core.Objects.Create
import Yaifl.Core.Objects.Room
import Yaifl.Test.Common
import Solitude
import Yaifl.Core.Objects.RoomData
import Yaifl.Core.AdaptiveNarrative
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Core.Rules.Rule
import Effectful.Writer.Static.Local

awnN :: Text' wm
awnN = "Awning"

awnDesc :: Text' wm
awnDesc =
  [wrappedText|A tan awning is stretched on tent poles over the dig-site, providing a little shade to the workers here; you are at the bottom of a square
  twenty feet on a side, marked out with pegs and lines of string. Uncovered in the south face of this square is an awkward opening into the earth.|]

swcN :: Text' wm
swcN = "Slightly Wrong Chamber"

swcDesc :: Text' wm
swcDesc = Text' $ Right ("description of slightly wrong chamber", RuleLimitedEffect $ do
  obj <- view #objectData <$> getMentionedRoom
  when (isVisited obj /= Visited) $
    tell [wrappedText|When you first step into the room, you are bothered by the sense that something is not quite right: perhaps the lighting,
          perhaps the angle of the walls. |]
  tell [wrappedText|A mural on the far wall depicts a woman with a staff, tipped with a pine-cone. She appears to be watching you.|]
  )

ex4World :: Game PlainWorldModel ()
ex4World = do
    setTitle "Verbosity"
    -- inform7 uses superbrief, brief, and verbose as the command words
    -- even though the BtS names are abbreviated, sometimes abbreviated, and not abbreviated
    --roomDescriptions .= SometimesAbbreviatedRoomDescriptions
    swc <- addRoom' swcN swcDesc pass
    awn <- addRoom' awnN awnDesc pass

    swc `isSouthOf` awn
    pass

ex4TestMeWith :: [Text]
ex4TestMeWith = []