module Yaifl.Test.Chapter3.PortRoyale where

import Yaifl
import Yaifl.Metadata
import Yaifl.Model.Object
import Yaifl.Model.Objects.Create
import Yaifl.Model.Objects.Effects
import Yaifl.Rules.RuleEffects
import Yaifl.Text.Print
import Yaifl.World
import Yaifl.Rules.Rule
import qualified Data.Text as T
import Solitude
import Data.Text.Display
import Yaifl.Model.WorldModel
import Yaifl.Test.Common

-- a combination of port royale:
-- part 1 https://ganelson.github.io/inform-website/book/WI_3_2.html
portRoyale :: Game PlainWorldModel ()
portRoyale = do
  setTitle "1691"
  fj <- addRoom' "Fort James" [wrappedText|The enclosure of Fort James is a large, roughly hexagonal court walled with heavy stone. The walls face the entrance to Port Royal Harbour, and the battery of guns
is prepared to destroy any enemy ship arriving.|] pass
  pass