{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Yaifl.Rules.Args
  ( Args(..)
  , Refreshable(..)
  , ActionParameter
  , NamedActionParameter(..)
  , ActionOptions(..)
  , UnverifiedArgs(..)
  , ArgumentParseResult
  , ActionParameterType(..)
  , GoesWith(..)
  --, withPlayerSource
  , getPlayer
  , getActorLocation
  --, blankArgs
  --, playerArgs
  --, getActionParameter
  --, getNoun
  ) where

import Solitude hiding (show)

import Effectful.Optics
import Yaifl.Metadata ( Timestamp, currentPlayer )
import Yaifl.Model.Object
import Yaifl.Model.WorldModel
import Data.Text.Display
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Objects.ObjectLike
import Yaifl.Model.Objects.Query
import GHC.Show
import qualified Data.Set as S

data ActionParameterType =
  TakesNoParameter
  | Optionally ActionParameterType
  | TakesDirectionParameter
  | TakesObjectParameter
  | TakesOneOf ActionParameterType ActionParameterType
  | TakesConstantParameter
  deriving stock (Show)

data NamedActionParameter wm =
  NoParameter
  | DirectionParameter (WMDirection wm)
  | ObjectParameter (AnyObject wm)
  | ConstantParameter Text
  deriving stock ( Generic)

instance Show (NamedActionParameter wm) where
  show = \case
    NoParameter -> "No parameter"
    DirectionParameter _ -> "direction"
    ObjectParameter _ -> "object"
    ConstantParameter t -> show t
deriving stock instance Eq (WMDirection wm) => Eq (NamedActionParameter wm)
deriving stock instance Ord (WMDirection wm) => Ord (NamedActionParameter wm)

type family ActionParameter wm (goesWith :: ActionParameterType) where
  ActionParameter wm TakesNoParameter = ()
  ActionParameter wm (Optionally goesWith) = Maybe (ActionParameter wm goesWith)
  ActionParameter wm TakesDirectionParameter = WMDirection wm
  ActionParameter wm TakesObjectParameter = AnyObject wm
  ActionParameter wm TakesConstantParameter = Text
  ActionParameter wm (TakesOneOf goesWith1 goesWith2) = Either (ActionParameter wm goesWith1) (ActionParameter wm goesWith2)

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

-- | Arguments for an action, activity, or rulebook.
data Args wm v = Args
  { source :: Thing wm
  , variables :: v
  , actionOptions :: ActionOptions wm
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

instance {-# OVERLAPPING #-} Refreshable wm v => Refreshable wm (Args wm v) where
  refreshVariables av = do
    v <- refreshVariables (variables av)
    o <- getThing (tagThing $ source av)
    return $ av { source = o, variables = v }

type ArgumentParseResult v = Either Text v

-- | Before 'Args' are parsed, the variable is just a command string
-- the action has to parse them, ideally into some intermediary mix of `ArgSubject`.
newtype UnverifiedArgs wm (goesWith :: ActionParameterType) = UnverifiedArgs
  { unArgs :: Args wm (ActionParameter wm goesWith, [(Text, NamedActionParameter wm)])
  } deriving newtype (Generic)

--instance Refreshable wm (UnverifiedArgs wm goesWith) where
--  refreshVariables = return

--deriving stock instance (WMEq wm) => Eq (UnverifiedArgs wm)
--deriving newtype instance (WMOrd wm) => Ord (UnverifiedArgs wm)

makeFieldLabelsNoPrefix ''Args

getNoun ::
  UnverifiedArgs wm goesWith
  -> ActionParameter wm goesWith
getNoun = fst . variables . unArgs

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
{-
withPlayerSource ::
  NoMissingObjects wm es
  => (Thing wm -> ActionParameter goesWith -> UnverifiedArgs wm goesWith)
  -> Eff es (Timestamp -> (ActionParameter goesWith, [NamedActionParameter wm]) -> UnverifiedArgs wm goesWith)
withPlayerSource = flip fmap getPlayer

-- | No Arguments, player source.
playerArgs ::
  forall wm es.
  NoMissingObjects wm es
  => Eff es (Timestamp -> ActionParameter goesWith ->  UnverifiedArgs wm goesWith)
playerArgs = do
  ua <- withPlayerSource blankArgs
  return (\ts a -> UnverifiedArgs { unArgs = ua ts (a, []) } )

blankArgs ::
  Thing wm
  -> ActionParameter goesWith
  -> UnverifiedArgs wm goesWith
blankArgs o g = UnverifiedArgs $ Args o (g, []) (ActionOptions False False) 0

argsWithArgument ::
  ActionParameter goesWith
  -> Thing wm
  -> UnverifiedArgs wm goesWith
argsWithArgument ap o = UnverifiedArgs $ Args o (ap, []) (ActionOptions False False) 0

getActionParameter ::
  UnverifiedArgs wm goesWith
  -> ActionParameter goesWith
getActionParameter (UnverifiedArgs (Args{variables})) = fst variables
-}
instance Functor (Args wm) where
  fmap f = #variables %~ f