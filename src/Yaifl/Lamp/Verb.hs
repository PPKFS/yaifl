{-# LANGUAGE UndecidableInstances #-}
module Yaifl.Lamp.Verb where

import GHC.TypeLits
import Solitude
import Yaifl.Core.AdaptiveNarrative


class ConjugatableVerb (v :: Symbol) where
  toVerb :: Proxy v -> Verb

instance {-# OVERLAPPABLE #-} (TypeError
  ('Text "Cannot conjugate the verb 'to " ':<>: 'ShowType a ':<>:
    'Text "'; perhaps you are missing a SayableValue (SayLiteral " ':<>:
    'ShowType a ':<>: 'Text ") instance if this is not a verb?")) => ConjugatableVerb a where
  toVerb :: Proxy a -> Verb
  toVerb = error "impossible"

data Voice = Active | Passive
data VerbSense = Positive | Negative

newtype Verb = Verb
  { tabulation :: VerbSense -> Voice -> Tense -> VerbPersonage -> Text }

makeVerb :: Text -> Verb
makeVerb "is" = Verb $ const $ const $ const $ \case
  FirstPersonSingular -> "am"
  ThirdPersonSingular -> "is"
  _ -> "are"
makeVerb "are" = makeVerb "is"
makeVerb x = Verb $ const $ const $ const $ const x
