module Yaifl.Model.Objects.Move
  ( move
  ) where

import Solitude

import Breadcrumbs
import Data.Text.Display
import Yaifl.Model.Entity ( HasID(..) )
import Yaifl.Metadata ( Metadata(..) )
import Yaifl.Model.Object
import Yaifl.Model.Objects.Query
import Yaifl.Model.Objects.ThingData
import Yaifl.Model.Properties.Enclosing
import Yaifl.Model.Properties.Has (WMHasProperty)
import Yaifl.Model.Properties.Query (getEnclosing, getPropertyOrThrow, setEnclosing)
import Yaifl.Model.WorldModel
import qualified Data.EnumSet as ES
import Yaifl.Model.Objects.Effects

move ::
  Breadcrumbs :> es
  => State Metadata :> es
  => ObjectQuery wm es
  => Display (WMSayable wm)
  => WMHasProperty wm Enclosing
  => ObjectLike wm o
  => Thing wm
  -> o
  -> Eff es Bool
move oObj oLoc = withoutMissingObjects moveBlock moveHandler
  where
    moveBlock = withSpan' "move" ""$ do
      o' <- refreshThing oObj
      loc <- getPropertyOrThrow "enclosing part of new location" oLoc =<< getEnclosing oLoc
      let c = o' ^. #objectData % #containedBy
      c' <- getObject c
      oLoc' <- getObject oLoc
      oldLocEnc <- getPropertyOrThrow "enclosing part of old location" c =<< getEnclosing c
      addTag "object to move" (display $ getID o')
      addTag "current location" (display $ getID  c')
      addTag "new location" (display $ getID oLoc')
      modifySpan (\s -> s { _spanName = display (oObj ^. #name) })
      let moveObjects newId t oldLoc newLocEncl = let (newLoc', t') = nowContains newId newLocEncl t in (t', oldLoc `noLongerContains` t, newLoc')
          noLongerContains cont obj = cont & (#contents %~ ES.delete (getID obj))
          nowContains contId cont obj = (cont & (#contents %~ ES.insert (getID obj)), obj & (#objectData % #containedBy .~ contId))
          (movedObj, oldLocation, newLocation) = moveObjects (getID oLoc) o' oldLocEnc loc
      setThing movedObj
      mapM_ (uncurry setEnclosing) [(c, oldLocation), (getID oLoc, newLocation)]
      --at this point we know it's a success
      return True
    moveHandler = handleMissingObject ("Failed to move " <> display (getID oObj) <> " to " <> display (getID oLoc)) False
