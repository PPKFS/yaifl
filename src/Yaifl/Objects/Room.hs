module Yaifl.Objects.Room where

import Yaifl.Directions
import Yaifl.Common
import Solitude
import Yaifl.Objects.ObjectData
import Yaifl.Objects.Missing
import Yaifl.WorldInfo
import Yaifl.Objects.Query
import Display
import Yaifl.Logger
import qualified Data.Text.Lazy.Builder as TLB
import Yaifl.Objects.Object


hasSpecificConnectionTo ::
  MonadWorld wm m
  => ObjectLike wm o
  => (Ord (WMDirections wm))
  => Explicitness 
  -> o
  -> WMDirections wm
  -> m (Maybe Entity)
hasSpecificConnectionTo expl o dir = do
  r <- getRoomMaybe o
  case r ^? _Just % objData % roomMapConnections % to unMapConnections % at dir % _Just of
    Just (Connection ex' e) 
      | ex' == expl -> return $ Just e
    _ -> return Nothing

isDirectionFrom ::
  NoMissingObjects m
  => MonadWorld wm m
  => (Ord (WMDirections wm))
  => m Entity
  -> Entity
  -> WMDirections wm
  -> m Entity
isDirectionFrom constrObj e dir = do
  -- run the construction
  mkObj <- constrObj
  -- ensure we have two rooms
  r1 <- getRoom e
  r0 <- getRoom mkObj
  -- we log a warning if we're in construction and we are overriding an explicit connection
  -- r0 is explicitly dir of r1; it is r1 we need to check
  -- r1 is implicitly (opposite dir) of r0.
  expl <- hasSpecificConnectionTo Explicit r1 dir
  whenConstructingM (isJust expl)
    -- TODO: this should be a nonblocking failure
    (warn $ TLB.fromText $ "Overriding an explicitly set map direction of room " <> displayText r1) 
  modifyRoom mkObj (makeExplicitConnection dir r0)
  whenConstr
  
  error ""

makeExplicitConnection :: WMDirections wm -> Room wm -> Room wm -> Room wm
makeExplicitConnection = error "not implemented"
whenConstructingM :: Bool -> m () -> m a1
whenConstructingM = error "not implemented"
