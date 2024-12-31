module Yaifl.Core.Actions.Args
  ( Args(..)
  , ArgsHaveMainObject(..)
  , ArgsMightHaveMainObject(..)
  , Refreshable(..)
  , ActionOptions(..)
  , UnverifiedArgs(..)
  , getActorLocation
  , silentAction
  , unlessSilent
  , normalAction
  ) where

import Yaifl.Prelude hiding (show)

import Yaifl.Core.Kinds.Object
import Yaifl.Core.Effects
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Kinds.Room
import Yaifl.Core.Refreshable
import Yaifl.Core.Query.Enclosing
import Yaifl.Core.Actions.GoesWith

data ActionOptions wm = ActionOptions
  { silently :: Bool
  , hidePrompt :: Bool
  } deriving stock (Eq, Ord, Generic)

-- | Arguments for an action, activity, or rulebook.
data Args wm v = Args
  { source :: Thing wm
  , variables :: v
  , actionOptions :: ActionOptions wm
  , timestamp :: Timestamp
  } deriving stock (Eq, Ord, Generic)

instance Display (Args wm v) where
  displayBuilder = const "args"

instance {-# OVERLAPPING #-} Refreshable wm v => Refreshable wm (Args wm v) where
  refresh av = do
    v <- refresh (variables av)
    o <- refresh $ source av
    return $ av { source = o, variables = v }

-- | Before 'Args' are parsed, the variable is just a command string
-- the action has to parse them, ideally into some intermediary mix of `ArgSubject`.
newtype UnverifiedArgs wm (goesWith :: ActionParameterType) = UnverifiedArgs
  { unArgs :: Args wm (ActionParameter wm goesWith, [(Text, NamedActionParameter wm)])
  } deriving newtype (Generic)

makeFieldLabelsNoPrefix ''Args
makeFieldLabelsNoPrefix ''UnverifiedArgs

instance Functor (Args wm) where
  fmap f = #variables %~ f

silentAction :: ActionOptions wm
silentAction = ActionOptions True True

normalAction :: ActionOptions wm
normalAction = ActionOptions False False

unlessSilent ::
  Applicative m
  => Args wm v
  -> m ()
  -> m ()
unlessSilent args = unless (silently . actionOptions $ args)
class ArgsHaveMainObject argVars obj | argVars -> obj where
  argsMainObject :: Lens' argVars obj

class ArgsMightHaveMainObject argVars obj | argVars -> obj where
  argsMainObjectMaybe :: AffineTraversal' argVars obj

instance ArgsHaveMainObject a a where
  argsMainObject = castOptic $ iso id id

instance (ArgsHaveMainObject vars o) => ArgsHaveMainObject (Args wm vars) o where
  argsMainObject = #variables % argsMainObject

instance (ArgsHaveMainObject vars o) => ArgsMightHaveMainObject (Args wm vars) o where
  argsMainObjectMaybe = #variables % argsMainObjectMaybe

instance {-# OVERLAPS #-} (ArgsHaveMainObject vars o) => ArgsMightHaveMainObject vars o where
  argsMainObjectMaybe = castOptic argsMainObject

-- alas, this throws issues with type families in instances
-- instance ArgsHaveMainObject (UnverifiedArgs wm goesWith) (ActionParameter wm goesWith)
getNoun ::
  UnverifiedArgs wm goesWith
  -> ActionParameter wm goesWith
getNoun = fst . variables . unArgs

getActorLocation ::
  NoMissingObjects wm es
  => Args wm v
  -> Eff es (Room wm)
getActorLocation args = getLocation $ source args
