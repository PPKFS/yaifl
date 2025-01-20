

module Yaifl.Core.Actions.GoesWith
  ( ActionParameterType(..)
  , ActionParameter
  , NamedActionParameter(..)
  , GoesWith(..)
  ) where

import Yaifl.Prelude hiding (show)

import Yaifl.Core.WorldModel
import GHC.Show
import qualified Data.Set as S
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Kinds.AnyObject

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
  default tryParseArguments :: Proxy g -> Set (NamedActionParameter wm) -> Maybe (ActionParameter wm g)
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