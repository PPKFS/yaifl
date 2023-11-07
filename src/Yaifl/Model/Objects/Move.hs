module Yaifl.Model.Objects.Move
  ( move
  ) where

import Solitude

import Breadcrumbs
import Data.Text.Display
import Yaifl.Model.Entity
import Yaifl.Metadata ( Metadata(..) )
import Yaifl.Model.Object
import Yaifl.Model.Objects.Query
import Yaifl.Model.Objects.ThingData
import Yaifl.Model.Properties.Enclosing
import Yaifl.Model.Properties.Has (WMHasProperty)
import Yaifl.Model.Properties.Query
import Yaifl.Model.WorldModel
import qualified Data.EnumSet as ES
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Objects.ObjectLike

move ::
  Breadcrumbs :> es
  => State Metadata :> es
  => ObjectQuery wm es
  => Display (WMSayable wm)
  => WMHasProperty wm Enclosing
  => ThingLike wm o
  => EnclosingLike wm l
  => ObjectLike wm l
  => o
  -> l
  -> Eff es Bool
move oObj oLoc = withoutMissingObjects moveBlock moveHandler
  where
    moveBlock = withSpan' "move" ""$ do
      o' <- refreshThing oObj
      (loc :: Enclosing) <- getAs oLoc
      let (c :: EnclosingEntity) = o' ^. #objectData % #containedBy
      c' <- getObject c
      oLoc' <- getObject oLoc
      (oldLocEnc :: Enclosing) <- getAs c
      addTag "object to move" (display $ getID o')
      addTag "current location" (display $ getID  c')
      addTag "new location" (display $ getID oLoc')
      modifySpan (\s -> s { _spanName = display (o' ^. #name) })
      let (movedObj, oldLocation, newLocation) = moveObjects (tag oldLocEnc (getID oLoc')) o' oldLocEnc loc
      setThing movedObj
      setEnclosing c oldLocation
      setEnclosing (getID oLoc) newLocation
      --at this point we know it's a success
      return True
    moveHandler = handleMissingObject ("Failed to move " <> display (getID oObj) <> " to " <> display (getID oLoc)) False

moveObjects :: EnclosingEntity -> Thing wm -> Enclosing -> Enclosing -> (Thing wm, Enclosing, Enclosing)
moveObjects newId t oldLoc newLocEncl = let (newLoc', t') = nowContains newId newLocEncl t in (t', oldLoc `noLongerContains` t, newLoc')
noLongerContains :: HasID n => Enclosing -> n -> Enclosing
noLongerContains cont obj = cont & (#contents %~ ES.delete (getID obj))
nowContains :: EnclosingEntity -> Enclosing -> Thing wm -> (Enclosing, Thing wm)
nowContains contId cont obj = (cont & (#contents %~ ES.insert (getID obj)), obj & (#objectData % #containedBy .~ contId))
