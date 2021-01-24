module Yaifl.Components
    ( module Yaifl.Components.Object
    --, module Yaifl.Components.Direction
    , module Yaifl.Components.Room
    , module Yaifl.Components.Enclosing
    , module Yaifl.Components.Physical
    , module Yaifl.Components.Player
    , HasStandardWorld
    ) where

--import           Yaifl.Components.Direction
import           Yaifl.Components.Object
import           Yaifl.Components.Room
import           Yaifl.Components.Enclosing
import           Yaifl.Components.Physical
import           Yaifl.Components.Player
import           Yaifl.Prelude
import           Yaifl.Common

type HasStandardWorld w m = (Monad m, HasThing w, HasRoom w, HasStore w Player) 