module Yaifl.Core.AdaptiveNarrative where


import Yaifl.Core.Object
import Solitude
import Effectful.Optics (use, (.=))
import Yaifl.Core.Objects.Query
import Yaifl.Core.Metadata
import Breadcrumbs
import Yaifl.Core.Rules.Args (getPlayer)

data NarrativeViewpoint =
  FirstPersonSingular
  | FirstPersonPlural
  | SecondPersonSingular
  | SecondPersonPlural
  | ThirdPersonPlural
  | ThirdPersonSingular

data Tense = Present | Past | Perfect | PastPerfect | Future

data AdaptiveNarrative wm = AdaptiveNarrative
  { narrativeViewpoint :: NarrativeViewpoint
  --, adaptiveTextViewpoint :: NarrativeViewpoint
  , tense :: Tense
  , priorNamedObject :: Maybe (AnyObject wm)
  } deriving stock (Generic)

blankAdaptiveNarrative :: AdaptiveNarrative wm
blankAdaptiveNarrative = AdaptiveNarrative
  { narrativeViewpoint = SecondPersonSingular
  --, adaptiveTextViewpoint = FirstPersonPlural
  , tense = Present
  , priorNamedObject = Nothing
  }

regarding ::
  State (AdaptiveNarrative wm) :> es
  => Maybe (AnyObject wm)
  -> Eff es ()
regarding mbObj = #priorNamedObject .= mbObj

regardingThePlayer ::
  forall wm es.
  State (AdaptiveNarrative wm) :> es
  => NoMissingObjects wm es
  => Eff es ()
regardingThePlayer = do
  p <- getPlayer @wm
  regarding $ Just $ toAny p

getMentioned ::
  State (AdaptiveNarrative wm) :> es
  => Eff es (Maybe (AnyObject wm))
getMentioned = use #priorNamedObject

getMentionedRoom ::
  forall wm es.
  State (AdaptiveNarrative wm) :> es
  => State Metadata :> es
  => ObjectLookup wm :> es
  => Breadcrumbs :> es
  => Eff es (Room wm)
getMentionedRoom = do
  (mbObj :: Maybe (AnyObject wm)) <- use @(AdaptiveNarrative wm) #priorNamedObject
  r <- join <$> forM mbObj getRoomMaybe
  case r of
    Nothing -> error "The last mentioned object was expected to be a room, but it was not"
    Just x -> pure x
