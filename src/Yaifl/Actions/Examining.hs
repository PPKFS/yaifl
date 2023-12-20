{-# LANGUAGE RecordWildCards #-}
module Yaifl.Actions.Examining where


import Yaifl.Actions.Action
import Solitude
import Yaifl.Rules.Args
import Yaifl.Rules.Rule
import Yaifl.Text.Responses
import Yaifl.Model.Object
import Yaifl.Model.Direction
import Yaifl.Model.WorldModel
import Yaifl.Rules.RuleEffects (SayableValue(..))
import Yaifl.Text.SayQQ

data ExaminingResponses wm = ER
  { examineDirectionA :: Response wm Direction
  , examineContainerA :: Response wm (Thing wm)
  , examineContainerB :: Response wm (Thing wm)
  , examineSupporterA :: Response wm (Thing wm)
  , examineDeviceA :: Response wm (Thing wm)
  , examineUndescribedA :: Response wm (Thing wm)
  } deriving stock (Generic)

notImplementedResponse :: Text -> Response wm a
notImplementedResponse t = Response $ const (sayTell t)

examiningResponsesImpl :: ExaminingResponses wm
examiningResponsesImpl = ER
  { examineDirectionA = notImplementedResponse "directionA"
  , examineContainerA = notImplementedResponse "containerA"
  , examineContainerB = notImplementedResponse "containerB"
  , examineSupporterA = notImplementedResponse "supporterA"
  , examineDeviceA = notImplementedResponse "deviceA"
  , examineUndescribedA = notImplementedResponse "undescribedA"
}

data ExaminingActionVariables wm = EAV
  { examining :: Either (WMDirection wm) (AnyObject wm)
  , examiningTextPrinted :: Bool
  }

type ExaminingAction wm = Action wm ('TakesOneOf 'TakesDirectionParameter 'TakesObjectParameter) (ExaminingActionVariables wm)
examiningAction :: WithResponseSet wm "examiningResponses" (ExaminingResponses wm) => ExaminingAction wm
examiningAction = Action
  "examining"
  ["examine", "examining", "look closely at"]
  []
  (ParseArguments (\(UnverifiedArgs Args{..}) -> do
    let examining = fst variables
    return $ Right $ EAV {examining, examiningTextPrinted = False}))
  (makeActionRulebook "before examining rulebook" [])
  (makeActionRulebook "check examining rulebook" [ actionRequiresLight ])
  (makeActionRulebook "carry out examining rulebook" [
    standardExamining
  , examineDirections
  , examineContainers
  , examineSupporters
  , examineDevices
  , examineUndescribed
  ])
  (makeActionRulebook "report examining rulebook" [ reportOtherPeopleExamining ])

type ExamineRule wm = Rule wm (Args wm (ExaminingActionVariables wm)) Bool
actionRequiresLight :: ExamineRule wm
actionRequiresLight = notImplementedRule "action requires light"

examineUndescribed :: WithResponseSet wm "examiningResponses" (ExaminingResponses wm) => ExamineRule wm
examineUndescribed = makeRule "examine undescribed things rule" forPlayer' $ \Args{..} -> do
  unless (examiningTextPrinted variables) $ sayResponse (#examiningResponses % #examineUndescribedA) (error "")
  rulePass

examineDevices :: ExamineRule wm
examineDevices = notImplementedRule "examine devices rule"

examineSupporters :: ExamineRule wm
examineSupporters = notImplementedRule "examine supporters rule"

examineContainers :: ExamineRule wm
examineContainers = notImplementedRule "examine containers rule"

examineDirections :: ExamineRule wm
examineDirections = notImplementedRule "examine directions rule"

standardExamining :: ExamineRule wm
standardExamining = Rule "standard examining rule" forPlayer' $ \Args{..} -> do
  -- if the noun provides the property description and the description of the noun is not "":
  _r <- whenRight Nothing (examining variables) $ \obj -> do
    desc <- sayText (view #description obj)
    -- say "[description of the noun][line break]";
    -- now examine text printed is true.
    if desc /= "" then
       do
        [sayingLn|{desc}|]
        pure $ Just True
      else pure Nothing
  pure (Nothing, Nothing)

reportOtherPeopleExamining :: ExamineRule wm
reportOtherPeopleExamining = notImplementedRule "report others examining rule"
