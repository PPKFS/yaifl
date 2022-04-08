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

-- | Arguments for an action, activity, or rulebook. These are parameterised over
-- the closed 's' universe and the variables, which are either unknown
-- (see 'UnverifiedArgs') or known (concrete instantation).
data Args wm v = Args
  { _argsSource :: Maybe (AnyObject wm)
  , _argsVariables :: v
  , _argsTimestamp :: Timestamp
  } deriving stock (Eq, Ord, Generic)

deriving stock instance (Show (ObjSpecifics wm), Show v) => Show (Args wm v)
deriving stock instance (Read (ObjSpecifics wm), Read v) => Read (Args wm v)

-- | Before 'Args' are parsed, the variables are a list of objects.
newtype UnverifiedArgs wm = UnverifiedArgs
  { unArgs :: Args wm [AnyObject wm]
  } deriving stock (Eq)
    deriving newtype (Ord, Generic)


makeLenses ''Args

instance Functor (Args wm) where
  fmap f = argsVariables %~ f