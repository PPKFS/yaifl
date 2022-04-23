{-|
Module      : Yaifl.Rulebooks.Args
Description : Rulebook arguments.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Rulebooks.Args where
  
import Yaifl.Objects.Object
import Solitude
import Yaifl.Common
import Yaifl.Objects.Missing
import Yaifl.WorldInfo
import Yaifl.Objects.Query ()

-- | Arguments for an action, activity, or rulebook. These are parameterised over
-- the closed 's' universe and the variables, which are either unknown
-- (see 'UnverifiedArgs') or known (concrete instantation).
data Args wm v = Args
  { _argsSource :: Thing wm
  , _argsVariables :: v
  , _argsTimestamp :: Timestamp
  } deriving stock (Eq, Ord, Generic)

deriving stock instance (WMShow wm, Show v) => Show (Args wm v)
deriving stock instance (WMRead wm, WMOrd wm, Read v) => Read (Args wm v)

-- | All of the objects in the arguments are READ-ONLY. Whilst they can be swapped out, the 
-- refreshVariables function is called to replace and update the objects
class Refreshable wm av where
  refreshVariables :: forall m. (NoMissingObjects m, MonadWorld wm m) => av -> m av

instance Refreshable wm () where
  refreshVariables = pure

data ArgSubject wm = 
  RegularSubject Entity -- GET LAMP
  | ConceptSubject Text -- TALK TO BOB ABOUT *PHILOSOPHY*
  | DirectionSubject (WMDirections wm)
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

deriving stock instance (WMEq wm) => Eq (UnverifiedArgs wm)
deriving stock instance (WMShow wm) => Show (UnverifiedArgs wm)
deriving newtype instance (WMOrd wm) => Ord (UnverifiedArgs wm)
deriving newtype instance (WMRead wm, WMOrd wm) => Read (UnverifiedArgs wm)

makeLenses ''Args

withPlayerSource :: 
  forall wm m. 
  NoMissingObjects m
  => MonadWorld wm m
  => (Thing wm -> UnverifiedArgs wm)
  -> m (UnverifiedArgs wm)
withPlayerSource = flip fmap getPlayer

-- | This should be moved somewhere else I guess
getPlayer :: 
  NoMissingObjects m
  => MonadWorld wm m
  => m (Thing wm)
getPlayer = do
  plID <- use currentPlayer
  getThing plID

-- | No Arguments, player source.
playerNoArgs ::
  forall wm m. 
  NoMissingObjects m
  => MonadWorld wm m
  => m (Timestamp -> UnverifiedArgs wm)
playerNoArgs = do
  ua <- withPlayerSource blankArgs
  return (\ts -> ua & coercedTo @(Args wm [ArgSubject wm]) % argsTimestamp .~ ts)

blankArgs :: 
  Thing wm
  -> UnverifiedArgs wm
blankArgs o = UnverifiedArgs $ Args o [] 0

instance Functor (Args wm) where
  fmap f = argsVariables %~ f

makePrisms ''ArgSubject