-- ~\~ language=Haskell filename=src/Yaifl/Objects/Move.hs
-- ~\~ begin <<lit/worldmodel/objects/move.md|src/Yaifl/Objects/Move.hs>>[0] project://lit/worldmodel/objects/move.md:6
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

-- ~\~ begin <<lit/worldmodel/objects/move.md|move-func>>[0] project://lit/worldmodel/objects/move.md:27
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
      -- ~\~ begin <<lit/worldmodel/objects/move.md|lookup-move>>[0] project://lit/worldmodel/objects/move.md:51
      o' <- getThing oObj
      loc <- getPropertyOrThrow "enclosing part of new location" oLoc =<< getEnclosing oLoc
      let c = o' ^. objData % thingContainedBy
      oldLocEnc <- getPropertyOrThrow "enclosing part of old location" c =<< getEnclosing c
      debug $ bformat ("Moving " %! stext %! " from " %! stext %! " to " %! stext) (o' ^. objName) (displayText c) (displayText (getID oLoc))
      -- ~\~ end
      -- ~\~ begin <<lit/worldmodel/objects/move.md|move-thing>>[0] project://lit/worldmodel/objects/move.md:59
      let moveObjects newId t oldLoc newLocEncl = let (newLoc', t') = nowContains newId newLocEncl t in (t', oldLoc `noLongerContains` t, newLoc')
          noLongerContains cont obj = cont & (enclosingContains %~ ES.delete (getID obj))
          nowContains contId cont obj = (cont & (enclosingContains %~ ES.insert (getID obj)), obj & (objData % thingContainedBy .~ contId))
          (movedObj, oldLocation, newLocation) = moveObjects (getID oLoc) o' oldLocEnc loc
      -- ~\~ end

      -- ~\~ begin <<lit/worldmodel/objects/move.md|update-move>>[0] project://lit/worldmodel/objects/move.md:66
      setThing movedObj
      mapM (uncurry setEnclosing) [(c, oldLocation), (getID oLoc, newLocation)] 
      tickGlobalTime True
      -- ~\~ end
      --at this point we know it's a success
      return $ True
    moveHandler = handleMissingObject 
      (bformat ("Failed to move ObjectID " %! int %! " to ObjectID " %! int ) (getID oObj) (getID oLoc)) $ return False
-- ~\~ end
-- ~\~ end
