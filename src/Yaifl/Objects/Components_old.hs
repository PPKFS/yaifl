module Yaifl.Components
  ( module Yaifl.Components.Object,
    --, module Yaifl.Components.Direction
    module Yaifl.Components.Room,
    module Yaifl.Components.Enclosing,
    module Yaifl.Components.Player,
    module Yaifl.Components.Container,
    module Yaifl.Components.Openable,
    module Yaifl.Components.Supporter,
    module Yaifl.Components.Enterable,
    HasStandardWorld,
    WithStandardWorld,
  )
where

--import           Yaifl.Components.Direction

import Yaifl.Common
import Yaifl.Components.Container
import Yaifl.Components.Enclosing
import Yaifl.Components.Enterable
import Yaifl.Components.Object
import Yaifl.Components.Openable
import Yaifl.Components.Player
import Yaifl.Components.Room
import Yaifl.Components.Supporter

type HasStandardWorld w = (HasThing w, HasRoom w, HasStore w Player, HasContainer w, HasStore w Supporter, HasStore w Enterable)

type WithStandardWorld w m = (WithGameData w m, HasStandardWorld w)
