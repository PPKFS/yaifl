# Moving Objects

This is the first piece of code where we interact with objects at a higher level than foundational, and therefore we aim to exhibit the three layer cake model. That is, we aim to make as much separation between the things in layer 2 (things that require our monadic context) and those in layer 3 (the pure functions that do the work). It results in slightly more verbose code for this case, but it does pave the way for better testing later on.

```haskell file=src/Yaifl/Objects/Move.hs
module Yaifl.Objects.Move 
  ( move
  ) where
import Cleff.State ( State )
import qualified Data.EnumSet as ES
import Display ( displayText )
import Solitude

import Yaifl.Common ( HasID(..), tickGlobalTime, Metadata (..) )
import Yaifl.Logger ( debug, Log )
import Yaifl.Objects.Object ( objData, objName )
import Yaifl.Objects.ObjectData ( thingContainedBy )
import Yaifl.Objects.Query
import Yaifl.Properties.Enclosing ( enclosingContains, Enclosing )
import Yaifl.Properties.Property ( WMHasProperty )
import Yaifl.Properties.Query ( getEnclosing, getPropertyOrThrow, setEnclosing )

<<move-func>>
```

```haskell id=move-func
move :: 
  State (Metadata wm) :> es
  => Log :> es
  => ObjectQuery wm :> es
  => WMHasProperty wm Enclosing
  => ObjectLike wm o1
  => ObjectLike wm o2
  => o1
  -> o2
  -> Eff es Bool
move oObj oLoc = withoutMissingObjects moveBlock moveHandler 
  where
    moveBlock = do
      <<lookup-move>>
      <<move-thing>>

      <<update-move>>
      --at this point we know it's a success
      return $ True
    moveHandler = handleMissingObject 
      (bformat ("Failed to move ObjectID " %! int %! " to ObjectID " %! int ) (getID oObj) (getID oLoc)) $ return False
```

```haskell id=lookup-move
o' <- getThing oObj
loc <- getPropertyOrThrow "enclosing part of new location" oLoc =<< getEnclosing oLoc
let c = o' ^. objData % thingContainedBy
oldLocEnc <- getPropertyOrThrow "enclosing part of old location" c =<< getEnclosing c
debug $ bformat ("Moving " %! stext %! " from " %! stext %! " to " %! stext) (o' ^. objName) (displayText c) (displayText (getID oLoc))
```

```haskell id=move-thing
let moveObjects newId t oldLoc newLocEncl = let (newLoc', t') = nowContains newId newLocEncl t in (t', oldLoc `noLongerContains` t, newLoc')
    noLongerContains cont obj = cont & (enclosingContains %~ ES.delete (getID obj))
    nowContains contId cont obj = (cont & (enclosingContains %~ ES.insert (getID obj)), obj & (objData % thingContainedBy .~ contId))
    (movedObj, oldLocation, newLocation) = moveObjects (getID oLoc) o' oldLocEnc loc
```

```haskell id=update-move
setThing movedObj
mapM (uncurry setEnclosing) [(c, oldLocation), (getID oLoc, newLocation)] 
tickGlobalTime True
```

