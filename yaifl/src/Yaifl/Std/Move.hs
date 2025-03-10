module Yaifl.Std.Move
  ( move
  , updateToContain
  ) where

import Yaifl.Prelude
import Breadcrumbs


import Yaifl.Core.Metadata ( Metadata(..) )
import Yaifl.Core.Kinds.Object
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Effects
import Yaifl.Core.Entity
import Yaifl.Core.Tag
import Yaifl.Core.Kinds.Enclosing
import Yaifl.Core.WorldModel
import qualified Data.EnumSet as ES
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Core.ObjectLike
import Yaifl.Core.Query.Enclosing
import Yaifl.Core.Refreshable

move ::
  forall l wm es.
  Breadcrumbs :> es
  => State Metadata :> es
  => ObjectQuery wm es
  => Display (WMText wm)
  => WMWithProperty wm Enclosing
  => IsEnclosingObject l
  => ObjectLike wm l
  => Thing wm
  -> l
  -> Eff es Bool
move objectToMove oLoc = failHorriblyIfMissing moveBlock
  where
    moveBlock = withSpan' "move" "" $ do
      objectToMove' <- refreshThing objectToMove
      let loc :: Enclosing = getEnclosing oLoc
      let (c :: EnclosingEntity) = thingContainedBy objectToMove'
      c' <- getObject c
      oLoc' <- getObject oLoc
      let (taggedEnc :: TaggedAnyEnclosing wm) = tagObject c c'
      let (oldLocEnc :: Enclosing) = getEnclosing taggedEnc
      addTag "object to move" (display $ objectToMove')
      addTag "current location" (display $ c')
      addTag "new location" (display $ oLoc')
      modifySpan (\s -> s { _spanName = display (objectToMove' ^. #name) })
      let (movedObj, oldLocation, newLocation) = moveObjects (tagEntity oldLocEnc (getID oLoc')) objectToMove' oldLocEnc loc
      setThing movedObj
      setEnclosing c' oldLocation
      setEnclosing oLoc' newLocation
      --at this point we know it's a success
      return True

moveObjects :: EnclosingEntity -> Thing wm -> Enclosing -> Enclosing -> (Thing wm, Enclosing, Enclosing)
moveObjects newId t oldLoc newLocEncl = let (newLoc', t') = nowContains newId newLocEncl t in (t', oldLoc `noLongerContains` t, newLoc')
noLongerContains :: Enclosing -> Thing wm -> Enclosing
noLongerContains cont obj = cont & (#contents %~ ES.delete (tagThingEntity obj))
nowContains :: EnclosingEntity -> Enclosing -> Thing wm -> (Enclosing, Thing wm)
nowContains contId cont obj = (cont & (#contents %~ ES.insert (tagThingEntity obj)), obj & (#objectData % #containedBy .~ contId))

updateToContain ::
  NoMissingObjects wm es
  => WMWithProperty wm Enclosing
  => AnyObject wm
  -> Enclosing
  -> Thing wm
  -> Eff es ()
updateToContain cont enc obj = do
  setEnclosing cont (enc & (#contents %~ ES.insert (tagThingEntity obj)))