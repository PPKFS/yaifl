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
import Yaifl.Model.Properties.Has
import Yaifl.Model.Properties.Query
import Yaifl.Model.WorldModel
import qualified Data.EnumSet as ES
import Yaifl.Model.Objects.Effects

move ::
  Breadcrumbs :> es
  => State Metadata :> es
  => ObjectQuery wm es
  => Display (WMSayable wm)
  => WMWithProperty wm Enclosing
  => EnclosingObject l
  => ObjectLike wm l
  => Thing wm
  -> l
  -> Eff es Bool
move objectToMove oLoc = failHorriblyIfMissing moveBlock
  where
    moveBlock = withSpan' "move" ""$ do
      objectToMove' <- refreshThing objectToMove
      let loc :: Enclosing = oLoc ^. enclosingL
      let (c :: EnclosingEntity) = objectToMove' ^. #objectData % #containedBy
      c' <- getObject c
      oLoc' <- getObject oLoc
      let (oldLocEnc :: Enclosing) = getEnclosing c c'
      addTag "object to move" (display $ getID objectToMove')
      addTag "current location" (display $ getID  c')
      addTag "new location" (display $ getID oLoc')
      modifySpan (\s -> s { _spanName = display (objectToMove' ^. #name) })
      let (movedObj, oldLocation, newLocation) = moveObjects (tag oldLocEnc (getID oLoc')) objectToMove' oldLocEnc loc
      setThing movedObj
      setEnclosing c' oldLocation
      setEnclosing oLoc' newLocation
      --at this point we know it's a success
      return True

moveObjects :: EnclosingEntity -> Thing wm -> Enclosing -> Enclosing -> (Thing wm, Enclosing, Enclosing)
moveObjects newId t oldLoc newLocEncl = let (newLoc', t') = nowContains newId newLocEncl t in (t', oldLoc `noLongerContains` t, newLoc')
noLongerContains :: HasID n => Enclosing -> n -> Enclosing
noLongerContains cont obj = cont & (#contents %~ ES.delete (getID obj))
nowContains :: EnclosingEntity -> Enclosing -> Thing wm -> (Enclosing, Thing wm)
nowContains contId cont obj = (cont & (#contents %~ ES.insert (getID obj)), obj & (#objectData % #containedBy .~ contId))
