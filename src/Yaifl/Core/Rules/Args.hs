{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Core.Rules.Args where

import Solitude

import Effectful.Optics
import Yaifl.Core.Entity ( HasID(..), Entity )
import Yaifl.Core.Metadata ( Timestamp, currentPlayer )
import Yaifl.Core.Object ( Thing, Room )
import Yaifl.Core.Objects.Query (NoMissingObjects, getThing, getLocation)
import Yaifl.Core.WorldModel
import Data.Text.Display

-- | Arguments for an action, activity, or rulebook. These are parameterised over
-- the closed 's' universe and the variables, which are either unknown
-- (see 'UnverifiedArgs') or known (concrete instantation).
data Args wm v = Args
  { source :: Thing wm
  , variables :: v
  , timestamp :: Timestamp
  } deriving stock (Eq, Ord, Generic)

instance Display (Args wm v) where
  displayBuilder = const "args"

-- | All of the objects in the arguments are READ-ONLY. Whilst they can be swapped out, the
-- refreshVariables function is called to replace and update the objects
class Refreshable wm av where
  refreshVariables :: forall es. (NoMissingObjects wm es) => av -> Eff es av

instance {-# OVERLAPPABLE #-} Refreshable wm av where
  refreshVariables = pure

instance Refreshable wm v => Refreshable wm (Args wm v) where
  refreshVariables av = do
    v <- refreshVariables (variables av)
    o <- getThing (getID $ source av)
    return $ av { source = o, variables = v }

data ArgSubject wm =
  RegularSubject Entity -- GET LAMP
  | ConceptSubject Text -- TALK TO BOB ABOUT *PHILOSOPHY*
  | DirectionSubject (WMDirection wm) -- GO WEST
  | MatchedSubject Text Entity -- GO *THROUGH DOOR*
  deriving stock (Generic)

deriving stock instance (WMEq wm) => Eq (ArgSubject wm)
deriving stock instance (WMShow wm) => Show (ArgSubject wm)
deriving stock instance (WMOrd wm) => Ord (ArgSubject wm)
deriving stock instance (WMRead wm) => Read (ArgSubject wm)

-- | Before 'Args' are parsed, the variables are a list of objects.
newtype UnverifiedArgs wm = UnverifiedArgs
  { unArgs :: Args wm [ArgSubject wm]
  } deriving newtype (Generic)

instance Refreshable wm (UnverifiedArgs wm) where
  refreshVariables = return

deriving stock instance (WMEq wm) => Eq (UnverifiedArgs wm)
deriving newtype instance (WMOrd wm) => Ord (UnverifiedArgs wm)

makeFieldLabelsNoPrefix ''Args

withPlayerSource ::
  NoMissingObjects wm es
  => (Thing wm -> UnverifiedArgs wm)
  -> Eff es (UnverifiedArgs wm)
withPlayerSource = flip fmap getPlayer

-- | This should be moved somewhere else I guess TODO
getPlayer ::
  NoMissingObjects wm es
  => Eff es (Thing wm)
getPlayer = use #currentPlayer >>= getThing

getActorLocation ::
  NoMissingObjects wm es
  => Args wm v
  -> Eff es (Room wm)
getActorLocation args = getLocation $ source args

-- | No Arguments, player source.
playerNoArgs ::
  forall wm es.
  NoMissingObjects wm es
  => Eff es (Timestamp -> UnverifiedArgs wm)
playerNoArgs = do
  ua <- withPlayerSource blankArgs
  return (\ts -> ua & coercedTo @(Args wm [ArgSubject wm]) % #timestamp .~ ts)

blankArgs ::
  Thing wm
  -> UnverifiedArgs wm
blankArgs o = UnverifiedArgs $ Args o [] 0

instance Functor (Args wm) where
  fmap f = #variables %~ f

makePrisms ''ArgSubject