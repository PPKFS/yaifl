

module Yaifl.Test.Chapter3.Verbosity where

import Yaifl
import Yaifl.Test.Common
import Solitude
import Yaifl.Game
import Yaifl.World
import Yaifl.Objects.Create
import Yaifl.Objects.Room

ex3World :: Game PlainWorldModel (World PlainWorldModel)
ex3World = newWorld $ do
    setTitle "Verbosity"
    -- inform7 uses superbrief, brief, and verbose as the command words
    -- even though the BtS names are abbreviated, sometimes abbreviated, and not abbreviated
    --roomDescriptions .= SometimesAbbreviatedRoomDescriptions 
    addThing' "Bic pen" "" pass
    w <- addRoom' "The Wilkie Memorial Research Wing"
      [wrappedText|
      The research wing was built onto the science building in 1967, when the college's finances were 
      good but its aesthetic standards at a local minimum. A dull brown corridor recedes both north 
      and south; drab olive doors open onto the laboratories of individual faculty members. The 
      twitchy fluorescent lighting makes the whole thing flicker, as though it might wink out of 
      existence at any moment.

      The Men's Restroom is immediately west of this point.|] pass
    
    addRoom' "The Men's Restroom" 
      [wrappedText|
      Well, yes, you really shouldn't be in here.  But the nearest women's room is on the other side of the building, 
      and at this hour you have the labs mostly to yourself. All the same, you try not to read any of the things 
      scrawled over the urinals which might have been intended in confidence.|] pass `isWestOf` w

ex3TestMeWith :: [Text]
ex3TestMeWith = ["west", "east", "verbose", "west"]

ex3Test :: [Text]
ex3Test = ["aaa"]
