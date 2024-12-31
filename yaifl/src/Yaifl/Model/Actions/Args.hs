{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Yaifl.Model.Actions.Args
  ( Args(..)
  , ArgsHaveMainObject(..)
  , ArgsMightHaveMainObject(..)
  , Refreshable(..)
  , ActionParameter
  , NamedActionParameter(..)
  , ActionOptions(..)
  , UnverifiedArgs(..)
  , ActionParameterType(..)
  , GoesWith(..)
  --, withPlayerSource
  , getPlayer
  , getPlayer'
  , getActorLocation
  , silentAction
  , unlessSilent
  , normalAction
  --, blankArgs
  --, playerArgs
  --, getActionParameter
  --, getNoun
  ) where

import Yaifl.Prelude hiding (show)

import Yaifl.Core.Kinds.Object
import Yaifl.Model.WorldModel
import Yaifl.Core.Effects
import GHC.Show
import qualified Data.Set as S
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Core.Kinds.Room
import Yaifl.Model.Query
import Yaifl.Model.Kinds.Person
import Yaifl.Core.Refreshable
import Yaifl.Core.Query.Enclosing

data ActionParameterType =
  TakesNoParameter
  | Optionally ActionParameterType
  | TakesDirectionParameter
  | TakesObjectParameter
  | TakesThingParameter
  | TakesOneOf ActionParameterType ActionParameterType
  | TakesConstantParameter
  deriving stock (Show)

data NamedActionParameter wm =
  NoParameter
  | DirectionParameter (WMDirection wm)
  | ObjectParameter (AnyObject wm)
  | ThingParameter (Thing wm)
  | ConstantParameter Text
  | PluralParameter [NamedActionParameter wm]
  deriving stock ( Generic )

instance Show (NamedActionParameter wm) where
  show = \case
  -- todo: better named stuff
    NoParameter -> "No parameter"
    DirectionParameter _ -> "direction"
    ObjectParameter _ -> "object"
    ConstantParameter t -> show t
    ThingParameter _ -> "thing"
    PluralParameter wm -> "Multiple " <> show wm <> "s"
deriving stock instance Eq (WMDirection wm) => Eq (NamedActionParameter wm)
deriving stock instance Ord (WMDirection wm) => Ord (NamedActionParameter wm)

type family ActionParameter wm (goesWith :: ActionParameterType) where
  ActionParameter wm TakesNoParameter = ()
  ActionParameter wm (Optionally goesWith) = Maybe (ActionParameter wm goesWith)
  ActionParameter wm TakesDirectionParameter = WMDirection wm
  ActionParameter wm TakesObjectParameter = AnyObject wm
  ActionParameter wm TakesThingParameter = Thing wm
  ActionParameter wm TakesConstantParameter = Text
  ActionParameter wm (TakesOneOf goesWith1 goesWith2) = Either (ActionParameter wm goesWith1) (ActionParameter wm goesWith2)
  --ActionParameter wm (PluralParameter goesWith) = [ActionParameter wm goesWith]

class GoesWith (g :: ActionParameterType) where
  goesWithA :: Proxy g -> ActionParameterType
  tryParseArguments :: Eq (WMDirection wm) => Proxy g -> Set (NamedActionParameter wm) -> Maybe (ActionParameter wm g)
  default tryParseArguments :: Eq (WMDirection wm) => Proxy g -> Set (NamedActionParameter wm) -> Maybe (ActionParameter wm g)
  tryParseArguments _ _ = Nothing

instance GoesWith 'TakesNoParameter where
  goesWithA _ = TakesNoParameter
  tryParseArguments _ s = if s == S.empty then Just () else Nothing

instance GoesWith a => GoesWith ('Optionally a) where
  goesWithA _ = Optionally (goesWithA (Proxy @a))
  tryParseArguments _ s = if s == S.empty then Just Nothing else Just <$> tryParseArguments (Proxy @a) s

instance GoesWith 'TakesDirectionParameter where
  goesWithA _ = TakesDirectionParameter
  tryParseArguments _ s = if S.size s == 1
    then (case S.findMin s of
      DirectionParameter d -> Just d
      _ -> Nothing) else Nothing

instance GoesWith 'TakesObjectParameter where
  goesWithA _ = TakesObjectParameter
  tryParseArguments _ s = if S.size s == 1
    then (case S.findMin s of
      ObjectParameter d -> Just d
      _ -> Nothing) else Nothing

instance GoesWith 'TakesThingParameter where
  goesWithA _ = TakesThingParameter
  tryParseArguments _ s = if S.size s == 1
    then (case S.findMin s of
      ThingParameter d -> Just d
      _ -> Nothing) else Nothing

instance GoesWith 'TakesConstantParameter where
  goesWithA _ = TakesConstantParameter
  tryParseArguments _ s = if S.size s == 1
    then (case S.findMin s of
      ConstantParameter d -> Just d
      _ -> Nothing) else Nothing

instance (GoesWith a, GoesWith b) => GoesWith ('TakesOneOf a b) where
  goesWithA _ = TakesOneOf (goesWithA (Proxy @a)) (goesWithA (Proxy @b))
  tryParseArguments _ s = (Left <$> tryParseArguments (Proxy @a) s) <|> (Right <$> tryParseArguments (Proxy @b) s)

data ActionOptions wm = ActionOptions
  { silently :: Bool
  , hidePrompt :: Bool
  } deriving stock (Eq, Ord, Generic)

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

instance Functor (Args wm) where
  fmap f = #variables %~ f
