module Yaifl.Move
  ( move
  , updateToContain
  ) where

import Yaifl.Prelude
import Breadcrumbs

import Yaifl.Object.Kind
import Yaifl.Thing.Kind
import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.Tag
import Yaifl.Enclosing.Kind
import qualified Data.EnumSet as ES
import Yaifl.AnyObject
import Yaifl.ObjectLike
import Yaifl.Enclosing.Query
import Yaifl.Refreshable
import Yaifl.Property.Has

move ::
  forall l wm es.
  WithoutMissingObjects wm es
  => WMWithProperty wm Enclosing
  => IsEnclosingObject l
  => ObjectLike wm l
  => Thing wm
  -> l
  -> Eff es Bool
move objectToMove oLoc = moveBlock
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
      let (movedObj, oldLocation, newLocation) = moveObjects (tagEntity oldLocEnc (getEntity oLoc')) objectToMove' oldLocEnc loc
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
  WithoutMissingObjects wm es
  => WMWithProperty wm Enclosing
  => AnyObject wm
  -> Enclosing
  -> Thing wm
  -> Eff es ()
updateToContain cont enc obj = do
  setEnclosing cont (enc & (#contents %~ ES.insert (tagThingEntity obj)))