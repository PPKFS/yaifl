module Yaifl.Test.Chapter3.Verbosity where

import Yaifl
import Yaifl.Metadata
import Yaifl.Model.Objects.Create
import Yaifl.Model.Objects.RoomConnections
import Yaifl.Test.Common
import Solitude
import Yaifl.Text.DynamicText

ex3 :: (Text, [Text], Game PlainWorldModel ())
ex3 = ("Verbosity", ex3TestMeWith, ex3World)

wmrwN :: DynamicText wm
wmrwN = "The Wilkie Memorial Research Wing"

wmrwDesc :: DynamicText wm
wmrwDesc =
  [wrappedText|The research wing was built onto the science building in 1967, when the college's finances were
good but its aesthetic standards at a local minimum. A dull brown corridor recedes both north
and south; drab olive doors open onto the laboratories of individual faculty members. The
twitchy fluorescent lighting makes the whole thing flicker, as though it might wink out of
existence at any moment.

The Men's Restroom is immediately west of this point.|]

tmrN :: DynamicText wm
tmrN = "The Men's Restroom"

tmrDesc :: DynamicText wm
tmrDesc = [wrappedText|Well, yes, you really shouldn't be in here. But the nearest women's room is on the other side of the building,
      and at this hour you have the labs mostly to yourself. All the same, you try not to read any of the things
      scrawled over the urinals which might have been intended in confidence.|]

ex3World :: Game PlainWorldModel ()
ex3World = do
    setTitle "Verbosity"
    w <- addRoom' wmrwN wmrwDesc pass
    tmr <- addRoom' tmrN tmrDesc pass
    tmr `isWestOf` w
    pass

ex3TestMeWith :: [Text]
ex3TestMeWith = ["west", "brief", "east", "verbose", "west"]