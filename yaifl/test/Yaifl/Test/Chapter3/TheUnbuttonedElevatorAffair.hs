module Yaifl.Test.Chapter3.TheUnbuttonedElevatorAffair where


import Yaifl.Prelude
import Yaifl
import Yaifl.Std.Actions.Going
import Yaifl.Std.Kinds.Direction
import Yaifl.Std.Create.Object
import Yaifl.Std.Create.RoomConnection
import Yaifl.Std.Create.Rule
import Yaifl.Text.SayableValue
import Yaifl.Test.Common
import Yaifl.Text.Say
import Yaifl.Core.ObjectLike
import Yaifl.Core.Rules.Rulebook

ex9 :: (Text, [Text], Game PlainWorldModel ())
ex9 = ("The Unbuttoned Elevator Affair", theUnbuttonedElevatorAffairTestMeWith, theUnbuttonedElevatorAffairWorld)

theUnbuttonedElevatorAffairWorld :: Game PlainWorldModel ()
theUnbuttonedElevatorAffairWorld = do
  setTitle "The Unbuttoned Elevator Affair"
  uh <- addRoom "UNCLE Headquarters" ! #description [wrappedText|The steel nerve-center of the free world's battle against the Technological Hierarchy for
the Removal of Undesirables and the Subjugation of Humanity. Being against technology, we have only a very simple elevator to the east.|]
  dfts <- addRoom "Del Floria's Tailor Shop" ! #description [wrappedText|Only trained anti-THRUSH agents recognise the booth in the east wall as a secret elevator.|]
  tse <- addRoom "The Secret Elevator" ! done
  tse `isEastOf` uh
  tse `isEastOf` dfts

  after #going [toTheRoom tse] "ag1" $ const $ do
    [saying|The doors automatically close, there is a rush of motion, and they open again.|]
    tseR <- getRoom tse
    -- if UNCLE Headquarters is mapped west of the Secret Elevator
    if getMapConnection West tseR == Just uh
    then
      -- now Del Floria's Tailor Shop is mapped west of the Secret Elevator;
      isNowMapped dfts West tse
    else
      -- otherwise now UNCLE Headquarters is mapped west of the Secret Elevator;
      isNowMapped uh West uh
    rulePass

theUnbuttonedElevatorAffairTestMeWith :: [Text]
theUnbuttonedElevatorAffairTestMeWith = ["east", "west", "east", "west"]
