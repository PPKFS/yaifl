

module Yaifl.Actions.GoesWith
  ( ActionSignature(..)
  , ActionExpects
  , ActionParameter(..)
  , GoesWith(..)
  ) where

import Yaifl.Prelude hiding (show)

import Yaifl.WorldModel
import GHC.Show
import qualified Data.Set as S
import Yaifl.Thing.Kind
import Yaifl.AnyObject

data ActionSignature =
  TakesNoParameter
  | Optionally ActionSignature
  | TakesDirectionParameter
  | TakesObjectParameter
  | TakesThingParameter
  | TakesOneOf ActionSignature ActionSignature
  | TakesConstantParameter
  deriving stock ( Show, Eq, Ord, Generic )

data ActionParameter wm =
  NoParameter
  | DirectionParameter (WMDirection wm)
  | ObjectParameter (AnyObject wm)
  | ThingParameter (Thing wm)
  | ConstantParameter Text
  | PluralParameter [ActionParameter wm]
  deriving stock ( Generic )

instance Show (ActionParameter wm) where
  show = \case
  -- todo: better named stuff
    NoParameter -> "No parameter"
    DirectionParameter _ -> "direction"
    ObjectParameter _ -> "object"
    ConstantParameter t -> show t
    ThingParameter _ -> "thing"
    PluralParameter wm -> "Multiple " <> show wm <> "s"
deriving stock instance Eq (WMDirection wm) => Eq (ActionParameter wm)
deriving stock instance Ord (WMDirection wm) => Ord (ActionParameter wm)

type family ActionExpects wm (goesWith :: ActionSignature) where
  ActionExpects wm TakesNoParameter = ()
  ActionExpects wm (Optionally goesWith) = Maybe (ActionExpects wm goesWith)
  ActionExpects wm TakesDirectionParameter = WMDirection wm
  ActionExpects wm TakesObjectParameter = AnyObject wm
  ActionExpects wm TakesThingParameter = Thing wm
  ActionExpects wm TakesConstantParameter = Text
  ActionExpects wm (TakesOneOf goesWith1 goesWith2) = Either (ActionExpects wm goesWith1) (ActionExpects wm goesWith2)
  --ActionExpects wm (PluralParameter goesWith) = [ActionExpects wm goesWith]

class GoesWith (g :: ActionSignature) where
  goesWithA :: Proxy g -> ActionSignature
  tryParseArguments :: Proxy g -> Set (ActionParameter wm) -> Maybe (ActionExpects wm g)
  default tryParseArguments :: Proxy g -> Set (ActionParameter wm) -> Maybe (ActionExpects wm g)
  tryParseArguments _ _ = Nothing

instance GoesWith 'TakesNoParameter where
  goesWithA _ = TakesNoParameter
  tryParseArguments _ s = if S.null s then Just () else Nothing

instance GoesWith a => GoesWith ('Optionally a) where
  goesWithA _ = Optionally (goesWithA (Proxy @a))
  tryParseArguments _ s = if S.null s then Just Nothing else Just <$> tryParseArguments (Proxy @a) s

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
      ThingParameter t -> Just (toAny t)
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