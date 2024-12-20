{-# LANGUAGE RecordWildCards #-}
module Yaifl.Game.Actions.Examining where

import Yaifl.Prelude

import Yaifl.Model.Action
import Yaifl.Model.Actions.Args
import Yaifl.Model.HasProperty (WMWithProperty)
import Yaifl.Model.Kinds
import Yaifl.Model.Kinds.Container
import Yaifl.Model.Query
import Yaifl.Model.Rules.Rulebook
import Yaifl.Model.WorldModel

import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.ListWriter
import Yaifl.Text.Responses
import Yaifl.Text.Say
import Yaifl.Model.Tag
import Yaifl.Model.Kinds.Supporter

data ExaminingResponses =
  ExamineDirectionA
  | ExamineContainerA
  | ExamineContainerB
  | ExamineSupporterA
  | ExamineDeviceA
  | ExamineUndescribedA
  deriving stock (Generic)

newtype ExaminingTarget wm = ET { unwrapTarget :: Either (WMDirection wm) (Thing wm) }
  deriving stock (Generic)

data ExaminingActionVariables wm = EAV
  { examiningSubject :: ExaminingTarget wm
  , examiningTextPrinted :: Bool
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ExaminingActionVariables
makeFieldLabelsNoPrefix ''ExaminingTarget

instance ArgsMightHaveMainObject (ExaminingActionVariables wm) (Thing wm) where
  argsMainObjectMaybe = #examiningSubject % coerced @(ExaminingTarget wm) @(Either (WMDirection wm) (Thing wm)) % _Right

type ExaminingAction wm = Action wm ExaminingResponses ('TakesOneOf 'TakesDirectionParameter 'TakesThingParameter) (ExaminingActionVariables wm)

type HasExaminingProperties wm =
  ( WithPrintingNameOfSomething wm
  , WMWithProperty wm Container
  , WithListWriting wm
  )

examiningAction :: HasExaminingProperties wm => ExaminingAction wm
examiningAction = (makeAction "examining")
  { understandAs = ["examine", "examining", "look closely at", "x"]
  , parseArguments = ParseArguments (\(UnverifiedArgs Args{..}) -> do
      let examiningSubject = ET $ fst variables
      return $ SuccessfulParse $ EAV {examiningSubject, examiningTextPrinted = False})
  , checkRules = makeActionRulebook "check examining rulebook" [ actionRequiresLight ]
  , carryOutRules = makeActionRulebook "carry out examining rulebook" [
        standardExamining
      , examineDirections
      , examineContainers
      , examineSupporters
      , examineDevices
      , examineUndescribed
      ]
  , reportRules = makeActionRulebook "report examining rulebook" [ reportOtherPeopleExamining ]
  , responses = \case
      ExamineUndescribedA -> Response $ \a -> do
        let noun = a ^. #variables % #examiningSubject % #unwrapTarget
        whenRight_ noun $ \noun' ->
          [saying|#{We} #{see} nothing special about {the noun'}.|]
      ExamineContainerA -> Response $ \a -> do
        let noun = a ^. #variables % #examiningSubject % #unwrapTarget
        whenRight_ noun $ \noun' ->
          [saying|In {the noun'} |]
      ExamineSupporterA -> Response $ \a -> do
        let noun = a ^. #variables % #examiningSubject % #unwrapTarget
        whenRight_ noun $ \noun' ->
          [saying|On {the noun'} |]
      ExamineContainerB -> Response $ \a -> do
        let noun = a ^. #variables % #examiningSubject % #unwrapTarget
        whenRight_ noun $ \noun' ->
          [saying|{The noun'} #{are} empty.|]
      _ -> error "not done response"
  }

type ExamineRule wm = ActionRule wm (ExaminingAction wm) (ExaminingActionVariables wm)

actionRequiresLight :: ExamineRule wm
actionRequiresLight = notImplementedRule "action requires light"

examineUndescribed :: ExamineRule wm
examineUndescribed = makeRule "examine undescribed things rule" forPlayer' $ \a@Args{..} -> do
  -- if examine text printed is false:
  -- say "[We] [see] nothing special about [the noun]." (A).
  unless (examiningTextPrinted variables) $ sayResponse ExamineUndescribedA a
  rulePass

examineDevices :: ExamineRule wm
examineDevices = notImplementedRule "examine devices rule"

examineSupporters :: forall wm. HasExaminingProperties wm => ExamineRule wm
examineSupporters = Rule "examine supporters rule" forPlayer' $ \a@Args{..} -> do
  let o = a ^? #variables % argsMainObjectMaybe
  flip (maybe (return (Nothing, Nothing))) (getSupporterMaybe =<< o) $ \sup -> do
    -- if something described which is not scenery is on the noun and something which
    -- is not the player is in the noun and the noun is not falsely-unoccupied:
    r <- getDescribableContents (sup ^. #enclosing)
    if not . null $ r then do
      -- say "On [the noun] " (A);
      sayResponse ExamineSupporterA a
      --    list the contents of the noun, as a sentence, tersely, not listing
      --    concealed items, prefacing with is/are;
      let objs = withContents @wm r
      [saying|{isAreA objs}|]
      say @Text "."
      -- now examine text printed is true;
      examiningTextNowTrue a
    else
      return (Nothing, Nothing)

examiningTextNowTrue :: Args wm (ExaminingActionVariables wm) -> Eff es (Maybe (Args wm (ExaminingActionVariables wm)), Maybe Bool)
examiningTextNowTrue a = return (Just $ a & #variables % #examiningTextPrinted .~ True, Nothing)

examineContainers :: forall wm. HasExaminingProperties wm => ExamineRule wm
examineContainers = Rule "examine containers rule" forPlayer' $ \a@Args{..} -> do
  let o = a ^? #variables % argsMainObjectMaybe
  flip (maybe (return (Nothing, Nothing))) o $ \obj ->
    -- if the noun is a container:
    -- if the noun is closed and the noun is opaque, make no decision;
    flip (maybe (return (Nothing, Nothing))) (getContainerMaybe =<< o) $ \cont -> do
      p <- getPlayer
      playerInObject <- enclosingContains (tag (cont ^. #enclosing) obj) p
      if isOpaqueClosedContainer cont then return (Nothing, Nothing) else do
        -- if something described which is not scenery is in the noun and something which
        -- is not the player is in the noun and the noun is not falsely-unoccupied:
        r <- getDescribableContents (cont ^. #enclosing)
        if not . null $ r then do
          -- say "In [the noun] " (A);
          sayResponse ExamineContainerA a
          --    list the contents of the noun, as a sentence, tersely, not listing
          --    concealed items, prefacing with is/are;
          let objs = withContents @wm r
          [saying|{isAreA objs}|]
          say @Text "."
          -- now examine text printed is true;
          examiningTextNowTrue a
        else do
          -- if the player is in the noun:
          -- make no decision;
          if playerInObject
          then
            return (Nothing, Nothing)
          else do
            -- say "[The noun] [are] empty." (B);
            unless (examiningTextPrinted variables) $ sayResponse ExamineContainerB a
            examiningTextNowTrue a

examineDirections :: ExamineRule wm
examineDirections = notImplementedRule "examine directions rule"

standardExamining :: ExamineRule wm
standardExamining = Rule "standard examining rule" forPlayer' $ \a@Args{..} -> do
  -- if the noun provides the property description and the description of the noun is not "":
  r <- whenRight Nothing (unwrapTarget $ examiningSubject variables) $ \obj -> do
    regarding (Just obj)
    desc <- sayText (view #description obj)
    -- say "[description of the noun][line break]";
    -- now examine text printed is true.
    if desc /= "" then
      do
        [saying|{desc}#{linebreak}|]
        pure $ Just True
      else pure Nothing
  if isJust r then examiningTextNowTrue a else return (Nothing, Nothing)

reportOtherPeopleExamining :: ExamineRule wm
reportOtherPeopleExamining = notImplementedRule "report others examining rule"
