{-# LANGUAGE RecordWildCards #-}
module Yaifl.Game.Actions.Examining where

import Yaifl.Model.Action
import Solitude
import Yaifl.Model.Actions.Args
import Yaifl.Model.Rules.Rulebook
import Yaifl.Model.Kinds.Object
import Yaifl.Model.WorldModel
import Yaifl.Text.Say
import Yaifl.Text.SayQQ
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Kinds.Device
import Yaifl.Model.HasProperty
import Yaifl.Text.AdaptiveNarrative
import Data.Text.Display
import Effectful.Optics
import Yaifl.Text.Verb (Tense(..))

data ExaminingResponses =
  ExamineDirectionA
  | ExamineContainerA
  | ExamineContainerB
  | ExamineSupporterA
  | ExamineDeviceA
  | ExamineUndescribedA
  deriving stock (Generic)

newtype ExaminingTarget wm = ET { unwrapTarget :: Either (WMDirection wm) (AnyObject wm) }

data ExaminingActionVariables wm = EAV
  { examiningSubject :: ExaminingTarget wm
  , examiningTextPrinted :: Bool
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ExaminingActionVariables

instance ArgsMightHaveMainObject (ExaminingActionVariables wm) (AnyObject wm) where
  argsMainObjectMaybe = #examiningSubject % coerced @(ExaminingTarget wm) @(Either (WMDirection wm) (AnyObject wm)) % _Right

type ExaminingAction wm = Action wm ExaminingResponses ('TakesOneOf 'TakesDirectionParameter 'TakesObjectParameter) (ExaminingActionVariables wm)
examiningAction :: WithPrintingNameOfSomething wm => WMWithProperty wm Device => ExaminingAction wm
examiningAction = (makeAction "examining")
  { understandAs = ["examine", "examining", "look closely at", "x"]
  , parseArguments = ParseArguments (\(UnverifiedArgs Args{..}) -> do
      let examiningSubject = ET $ fst variables
      return $ Right $ EAV {examiningSubject, examiningTextPrinted = False})
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
  }

type ExamineRule wm = ActionRule wm (ExaminingAction wm) (ExaminingActionVariables wm)

actionRequiresLight :: ExamineRule wm
actionRequiresLight = notImplementedRule "action requires light"

examineUndescribed :: ExamineRule wm
examineUndescribed = makeRule "examine undescribed things rule" forPlayer' $ \Args{..} -> do
  --unless (examiningTextPrinted variables) $ sayResponse (#examiningResponses % #examineUndescribedA) (error "")
  rulePass

examineDevices :: forall wm. (WithPrintingNameOfSomething wm, WMWithProperty wm Device) => ExamineRule wm
examineDevices = Rule "examine devices rule" forPlayer' $ forMainObject $ \obj ->
  forKindOfThing obj getDeviceMaybe $ \thing device -> do
    let isOn = device ^. #switchedOn
    present <- (== Present) <$> use @(AdaptiveNarrative wm) #tense
    [saying|{The thing} #{are} {?if present}currently {?end if}switched {?if isOn}on{?else}off{?end if}.|]
    pure (Nothing, Nothing)

examineSupporters :: ExamineRule wm
examineSupporters = notImplementedRule "examine supporters rule"

examineContainers :: ExamineRule wm
examineContainers = notImplementedRule "examine containers rule"

examineDirections :: ExamineRule wm
examineDirections = notImplementedRule "examine directions rule"

standardExamining :: ExamineRule wm
standardExamining = Rule "standard examining rule" forPlayer' $ \Args{..} -> do
  -- if the noun provides the property description and the description of the noun is not "":
  _r <- whenRight Nothing (unwrapTarget $ examiningSubject variables) $ \obj -> do
    desc <- sayText (view #description obj)
    -- say "[description of the noun][line break]";
    -- now examine text printed is true.
    if desc /= "" then
      do
        [saying|{desc}#{linebreak}|]
        pure $ Just True
      else pure Nothing
  pure (Nothing, Nothing)

reportOtherPeopleExamining :: ExamineRule wm
reportOtherPeopleExamining = notImplementedRule "report others examining rule"
