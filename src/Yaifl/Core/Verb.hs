{-# LANGUAGE UndecidableInstances #-}
module Yaifl.Core.Verb where

import GHC.TypeLits
import Solitude

-- https://ganelson.github.io/inform/inflections-module/4-ei.html#SP14
--  Each set contains five tenses, which in English are present (1), past (2), perfect (3), past perfect (4) and future (5).
{-
define BASE_FORM_TYPE 0
define INFINITIVE_FORM_TYPE 1
define PRESENT_PARTICIPLE_FORM_TYPE 2
define PAST_PARTICIPLE_FORM_TYPE 3
define ADJOINT_INFINITIVE_FORM_TYPE 4
english has 5 (present) and 6 (past) too
-}
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

data Tense = Present | Past | Perfect | PastPerfect | Future
  deriving stock (Enum, Bounded, Generic, Eq, Show, Ord)

data VerbPersonage =
  FirstPersonSingular
  | FirstPersonPlural
  | SecondPersonSingular
  | SecondPersonPlural
  | ThirdPersonPlural
  | ThirdPersonSingular
  deriving stock (Enum, Bounded, Generic, Eq, Show, Ord)

data Verb = Verb
  { verbForms :: VerbForms
  , tabulation :: Tabulation
  }

data VerbForms = VerbForms
  { infinitive :: Text
  , presentParticiple :: Text
  , pastParticiple :: Text
  , adjointInfinitive :: Text
  , presentForm :: Text
  , pastForm :: Text
  }
  , tabulation :: Tabulation }

newtype Tabulation = Tabulation { runTabulation :: Voice -> Tense -> VerbSense -> VerbPersonage -> Text }

type VerbForms = Tabulation -> Verb

makeVerbForms :: Text -> VerbForms
makeVerbForms "be" = error ""
makeVerbForms x = error ""

makeVerb :: Text -> Tabulation
makeVerb "is" = Tabulation $ const $ const $ const $ \case
  FirstPersonSingular -> "am"
  ThirdPersonSingular -> "is"
  _ -> "are"
makeVerb "are" = makeVerb "is"
makeVerb x = regularVerbConjugation x

toBe :: Tabulation
toBe = makeVerb "is"

toDo :: Tabulation
toDo = makeVerb "do"

toAuxiliaryHave :: Tabulation
toAuxiliaryHave = makeVerb "auxiliary-have"

regularVerbConjugation :: Text -> Tabulation
regularVerbConjugation v = Tabulation $ \case
  Passive -> \vs t vp -> runTabulation toBe Passive vs t vp #| pastParticiple v #| "by"
  Active -> \case
    Present -> \case
      Positive -> regularVerbPresent v
      Negative -> \vp -> runTabulation toDo Active Present Negative vp #| infinitive v
    Past -> \case
      Positive -> const $ pastForm v
      Negative -> \vp -> runTabulation toDo Active Past Negative vp #| infinitive v
    Perfect -> \vs vp -> runTabulation toAuxiliaryHave Active Present vs vp #| pastParticiple v
    PastPerfect -> \vs vp -> runTabulation toAuxiliaryHave Active Past vs vp #| pastParticiple v
    Future -> let f pref = const $ pref #| infinitive v in \case
      Positive -> f "will"
      Negative -> f "will not"

(#|) :: Text -> Text -> Text
(#|) a b = a <> " " <> b

regularVerbPresent :: Text -> VerbPersonage -> Text
regularVerbPresent v = \case
  ThirdPersonSingular -> presentForm v
  _ -> infinitive v
