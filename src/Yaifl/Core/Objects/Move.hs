module Yaifl.Core.Objects.Move
  ( move
  ) where


import Yaifl.Core.Entity ( HasID(..) )
import Yaifl.Core.Logger ( debug, Log )
import Yaifl.Core.Metadata (tickGlobalTime, Metadata (..))
import Yaifl.Core.Object ( objData, objName, Thing )
import Yaifl.Core.Objects.Query
import Yaifl.Core.Objects.ThingData ( thingContainedBy )
import Yaifl.Core.Properties.Enclosing (enclosingContains, Enclosing)
import Yaifl.Core.Properties.Has (WMHasProperty)
import Yaifl.Core.Properties.Query (getEnclosing, getPropertyOrThrow, setEnclosing)
import qualified Data.EnumSet as ES
import Solitude
import Effectful.State.Static.Shared
import Effectful

move ::
  State Metadata :> es
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
      o' <- refreshThing oObj
      loc <- getPropertyOrThrow "enclosing part of new location" oLoc =<< getEnclosing oLoc
      let c = o' ^. objData % thingContainedBy
      oldLocEnc <- getPropertyOrThrow "enclosing part of old location" c =<< getEnclosing c
      debug [int|t| Moving #{o' ^. objName} from #{c} to #{getID oLoc}|]
      let moveObjects newId t oldLoc newLocEncl = let (newLoc', t') = nowContains newId newLocEncl t in (t', oldLoc `noLongerContains` t, newLoc')
          noLongerContains cont obj = cont & (enclosingContains %~ ES.delete (getID obj))
          nowContains contId cont obj = (cont & (enclosingContains %~ ES.insert (getID obj)), obj & (objData % thingContainedBy .~ contId))
          (movedObj, oldLocation, newLocation) = moveObjects (getID oLoc) o' oldLocEnc loc
      setThing movedObj
      mapM_ (uncurry setEnclosing) [(c, oldLocation), (getID oLoc, newLocation)]
      tickGlobalTime True
      --at this point we know it's a success
      return True
    moveHandler = handleMissingObject
      [int|t| Failed to move #{getID oObj} to #{getID oLoc}|] False