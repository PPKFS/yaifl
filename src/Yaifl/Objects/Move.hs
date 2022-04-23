{-|
Module      : Yaifl.Actions.Action
Description : An action is a verb that is carried out by the player (or an NPC).
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}


module Yaifl.Objects.Move where
import Yaifl.Objects.Object
import Yaifl.Properties.Property
import Yaifl.WorldInfo
import Yaifl.Properties.Enclosing
import Solitude
import qualified Data.Text.Lazy.Builder as TLB
import Yaifl.Objects.Missing
import qualified Data.EnumSet as ES
import Yaifl.Common
import Control.Monad.Except
import Yaifl.Properties.Query
import Yaifl.Logger
import Yaifl.Objects.ObjectData
import Yaifl.Objects.Query

move :: 
  MonadWorld wm m
  => WMHasProperty wm Enclosing
  => ObjectLike wm o1
  => ObjectLike wm o2
  => o1
  -> o2
  -> m Bool
move eObj eLoc = withoutMissingObjects (do
  -- reify both objects
  o' <- getThing eObj
  loc <- isJust <$> getEnclosing eLoc
  debug $ TLB.fromText (display o')
  if 
    loc
  then
    isJust <$> (do
      -- obtain the current container of the thing
      let container = o' ^. containedBy
      oName <- objectName o'
      contName <- objectName container
      eLocName <- objectName eLoc
      debug $ bformat ("Moving " %! stext %! " from " %! stext %! " to " %! stext) oName contName eLocName
      -- update the old location
      container `noLongerContains` o'
      -- update the thing moved
      eLoc `nowContains` o'
      tickGlobalTime True
      return $ Just True)
  else
    throwError $ MissingObject "Could not find enclosing part of location." (getID eLoc))
    (handleMissingObject (bformat ("Failed to move ObjectID " %! int %! " to ObjectID " %! int ) (getID eObj) (getID eLoc)) $ return False)

display :: Thing wm -> Text
display = error "not implemented"

noLongerContains ::
  NoMissingObjects m
  => MonadWorld wm m
  => WMHasProperty wm Enclosing
  => ObjectLike wm cont
  => ObjectLike wm obj
  => cont
  -> obj
  -> m ()
noLongerContains cont obj = modifyEnclosing cont
  (enclosingContains %~ ES.delete (getID obj))

nowContains :: 
  NoMissingObjects m
  => MonadWorld wm m
  => WMHasProperty wm Enclosing
  => ObjectLike wm cont
  => ObjectLike wm obj
  => cont
  -> obj
  -> m ()
nowContains cont obj = do
  modifyEnclosing cont (enclosingContains %~ ES.insert (getID obj))
  modifyThing obj (objData % thingContainedBy .~ getID cont)

getLocation
  :: NoMissingObjects m
  => MonadWorld wm m
  => ObjectLike wm o
  => o
  -> m Entity
getLocation o = do
  v <- getThing o
  let enclosedby = v ^. containedBy
  if
    isRoom enclosedby
  then
    return enclosedby
  else
    getLocation enclosedby