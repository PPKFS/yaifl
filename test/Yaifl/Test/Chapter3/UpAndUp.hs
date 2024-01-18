module Yaifl.Test.Chapter3.UpAndUp where

import Yaifl
import Yaifl.Model.Metadata
import Yaifl.Game.Create.Object
import Solitude
import Yaifl.Test.Common
import Yaifl.Game.Create.RoomConnection
import Yaifl.Text.Say
import Yaifl.Text.SayQQ
import Yaifl.Game.Create.Rule
import Yaifl.Game.Actions.Going
import Yaifl.Model.Rules.Rulebook

ex6 :: (Text, [Text], Game PlainWorldModel ())
ex6 = ("Up and Up", upAndUpTestMeWith, upAndUp)

upAndUp :: Game PlainWorldModel ()
upAndUp = do
  setTitle "Up and Up"
  pots <- addRoom "The Plain of the Skull" [wrappedText|A vast and trackless plain, enlivened only by the bones of those who have previously
tried and failed to cross. Above you is the Endless Tower, which rises half-way to the moon.|]
  et <- addRoom "Endless Tower" [wrappedText|From up here the Plain of the Skull seems only a small bald patch: the world is round and
most of it is covered with trees. Far off to the southwest is a shimmering surface that might be water; but there are no signs of
cities or civilizations, only the lizard-skeletons.|]
  pots `isBelow` et

  before (ActionRule #going) [toTheRoom et] "before climbing rule" $ \_ -> do
    [saying|You climb... and climb... and climb... The sun sets. The moon rises. The wind begins to blow. You continue to climb...|]
    rulePass

upAndUpTestMeWith :: [Text]
upAndUpTestMeWith = ["look", "up"]
