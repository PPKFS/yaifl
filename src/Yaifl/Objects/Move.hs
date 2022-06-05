-- ~\~ language=Haskell filename=src/Yaifl/Objects/Move.hs
-- ~\~ begin <<lit/worldmodel/objects/move.md|src/Yaifl/Objects/Move.hs>>[0] project://lit/worldmodel/objects/move.md:4
module Yaifl.Objects.Move where
import Yaifl.Objects.Object ( objData, objName, Thing )
import Yaifl.Properties.Property ( WMHasProperty )
import Yaifl.Properties.Enclosing ( enclosingContains, Enclosing )
import Solitude
import qualified Data.EnumSet as ES
import Yaifl.Common ( HasID(..), Entity, tickGlobalTime, Metadata (..) )
import Yaifl.Logger ( debug, Log )
import Yaifl.Objects.ObjectData ( thingContainedBy )
import Yaifl.Objects.Query
import Yaifl.Properties.Query ( getEnclosing, getPropertyOrThrow, setEnclosing )
import Cleff.State
import Display

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
move oObj oLoc = withoutMissingObjects (do
  -- process the objects
  o' <- getThing oObj
  loc <- getPropertyOrThrow "enclosing part of new location" oLoc =<< getEnclosing oLoc
  let c = o' ^. objData % thingContainedBy
  oldLocEnc <- getPropertyOrThrow "enclosing part of old location" c =<< getEnclosing c
  debug $ bformat ("Moving " %! stext %! " from " %! stext %! " to " %! stext) (o' ^. objName) (displayText c) (displayText (getID oLoc))
  -- at this point we know everything is well-formed and we are going to succeed
  let (movedObj, oldLocation, newLocation) = moveObjects (getID oLoc) o' oldLocEnc loc
  -- update them
  setThing movedObj
  mapM (uncurry setEnclosing) [(c, oldLocation), (getID oLoc, newLocation)] 
  tickGlobalTime True
  return $ True)
  (handleMissingObject (bformat ("Failed to move ObjectID " %! int %! " to ObjectID " %! int ) (getID oObj) (getID oLoc)) $ return False)

moveObjects :: Entity -> Thing wm -> Enclosing -> Enclosing -> (Thing wm, Enclosing, Enclosing)
moveObjects newId t oldLoc newLocEncl = (t', oldLoc `noLongerContains` t, newLoc')
  where
    (newLoc', t') = nowContains newId newLocEncl t

noLongerContains :: Enclosing -> Thing wm -> Enclosing
noLongerContains cont obj = cont & (enclosingContains %~ ES.delete (getID obj))

nowContains :: Entity -> Enclosing -> Thing wm -> (Enclosing, Thing wm)
nowContains contId cont obj = (cont & (enclosingContains %~ ES.insert (getID obj)), obj & (objData % thingContainedBy .~ contId))
{-
getLocation
  :: NoMissingObjects wm es
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
    -}
-- ~\~ end
