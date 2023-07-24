module Yaifl.Test.Chapter3.Verbosity where

import Yaifl
import Yaifl.Metadata
import Yaifl.Model.Objects.Create
import Yaifl.Model.Objects.RoomConnections
import Yaifl.Test.Common
import Solitude

wmrwN :: Text' wm
wmrwN = "The Wilkie Memorial Research Wing"

wmrwDesc :: Text' wm
wmrwDesc =
  [wrappedText|The research wing was built onto the science building in 1967, when the college's finances were
      good but its aesthetic standards at a local minimum. A dull brown corridor recedes both north
      and south; drab olive doors open onto the laboratories of individual faculty members. The
      twitchy fluorescent lighting makes the whole thing flicker, as though it might wink out of
      existence at any moment.

      The Men's Restroom is immediately west of this point.|]

tmrN :: Text' wm
tmrN = "The Men's Restroom"

tmrDesc :: Text' wm
tmrDesc = [wrappedText|Well, yes, you really shouldn't be in here.  But the nearest women's room is on the other side of the building,
      and at this hour you have the labs mostly to yourself. All the same, you try not to read any of the things
      scrawled over the urinals which might have been intended in confidence.|]

ex3World :: Game PlainWorldModel ()
ex3World = do
    setTitle "Verbosity"
    -- inform7 uses superbrief, brief, and verbose as the command words
    -- even though the BtS names are abbreviated, sometimes abbreviated, and not abbreviated
    --roomDescriptions .= SometimesAbbreviatedRoomDescriptions
    w <- addRoom' wmrwN wmrwDesc pass
    tmr <- addRoom' tmrN tmrDesc pass
    tmr `isWestOf` w
    pass

ex3TestMeWith :: [Text]
ex3TestMeWith = ["west", "east", "verbose", "west"]
{-
ex3Test :: [Text]
ex3Test = [
  expectLooking wmrwN wmrwDesc
  , expectAction "west"
  , expectLooking tmrN tmrDesc
  , expectAction "east"
  , expectLooking wmrwN wmrwDesc
  , expectAction "verbose"
  , expectAction "west"
  , expectLooking tmrN ""
  ]
-}