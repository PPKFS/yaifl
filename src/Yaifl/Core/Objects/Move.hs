-- ~\~ language=Haskell filename=src/Yaifl/Core/Objects/Move.hs
-- ~\~ begin <<lit/worldmodel/objects/move.md|src/Yaifl/Core/Objects/Move.hs>>[0] project://lit/worldmodel/objects/move.md:6
module Yaifl.Core.Objects.Move 
  ( move
  ) where
import Cleff.State ( State )
import qualified Data.EnumSet as ES


import Yaifl.Core.Common ( HasID(..), tickGlobalTime, Metadata (..) )
import Yaifl.Core.Logger ( debug, Log )
import Yaifl.Core.Objects.Object ( objData, objName, Thing )
import Yaifl.Core.Objects.ObjectData ( thingContainedBy )
import Yaifl.Core.Objects.Query
import Yaifl.Core.Properties.Enclosing ( enclosingContains, Enclosing )
import Yaifl.Core.Properties.Property ( WMHasProperty )
import Yaifl.Core.Properties.Query ( getEnclosing, getPropertyOrThrow, setEnclosing )
import Text.Interpolation.Nyan ( int, rmode' )

-- ~\~ begin <<lit/worldmodel/objects/move.md|move-func>>[0] project://lit/worldmodel/objects/move.md:27
move :: 
  State (Metadata wm) :> es
  => Log :> es
  => ObjectQuery wm es
  => WMHasProperty wm Enclosing
  => ObjectLike wm o
  => Thing wm
  -> o
  -> Eff es Bool
move oObj oLoc = withoutMissingObjects moveBlock moveHandler 
  where
    moveBlock = do
      -- ~\~ begin <<lit/worldmodel/objects/move.md|lookup-move>>[0] project://lit/worldmodel/objects/move.md:51
      o' <- refreshThing oObj
      loc <- getPropertyOrThrow "enclosing part of new location" oLoc =<< getEnclosing oLoc
      let c = o' ^. objData % thingContainedBy
      oldLocEnc <- getPropertyOrThrow "enclosing part of old location" c =<< getEnclosing c
      debug [int|t| Moving #{o' ^. objName} from #{c} to #{getID oLoc}|]
      -- ~\~ end
      -- ~\~ begin <<lit/worldmodel/objects/move.md|move-thing>>[0] project://lit/worldmodel/objects/move.md:59
      let moveObjects newId t oldLoc newLocEncl = let (newLoc', t') = nowContains newId newLocEncl t in (t', oldLoc `noLongerContains` t, newLoc')
          noLongerContains cont obj = cont & (enclosingContains %~ ES.delete (getID obj))
          nowContains contId cont obj = (cont & (enclosingContains %~ ES.insert (getID obj)), obj & (objData % thingContainedBy .~ contId))
          (movedObj, oldLocation, newLocation) = moveObjects (getID oLoc) o' oldLocEnc loc
      -- ~\~ end

      -- ~\~ begin <<lit/worldmodel/objects/move.md|update-move>>[0] project://lit/worldmodel/objects/move.md:66
      setThing movedObj
      mapM_ (uncurry setEnclosing) [(c, oldLocation), (getID oLoc, newLocation)] 
      tickGlobalTime True
      -- ~\~ end
      --at this point we know it's a success
      return True
    moveHandler = handleMissingObject 
      [int|t| Failed to move #{getID oObj} to #{getID oLoc}|] False
-- ~\~ end
-- ~\~ end
