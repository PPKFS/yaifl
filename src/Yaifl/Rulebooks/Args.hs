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
  { _argsSource :: Maybe (AnyObject wm)
  , _argsVariables :: v
  , _argsTimestamp :: Timestamp
  } deriving stock (Eq, Ord, Generic)

deriving stock instance (WMShow wm, Show v) => Show (Args wm v)
deriving stock instance (WMRead wm, WMOrd wm, Read v) => Read (Args wm v)

-- | Before 'Args' are parsed, the variables are a list of objects.
newtype UnverifiedArgs wm = UnverifiedArgs
  { unArgs :: Args wm [AnyObject wm]
  } deriving stock (Eq)
    deriving newtype (Ord, Generic)


makeLenses ''Args

withPlayerSource :: 
  forall wm m. 
  NoMissingObjects m
  => MonadWorld wm m
  => UnverifiedArgs wm
  -> m (UnverifiedArgs wm)
withPlayerSource u = do
  p <- getPlayer
  return $ u & coercedTo @(Args wm [AnyObject wm]) % argsSource ?~ review _Thing p

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
  return (\ts -> ua & coercedTo @(Args wm [AnyObject wm]) % argsTimestamp .~ ts)

blankArgs :: UnverifiedArgs wm
blankArgs = UnverifiedArgs $ Args Nothing [] 0

instance Functor (Args wm) where
  fmap f = argsVariables %~ f