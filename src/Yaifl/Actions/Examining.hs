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

data ExaminingActionVariables wm = EAV
  { examining :: Either (WMDirection wm) (AnyObject wm)
  , examiningTextPrinted :: Bool

  }

type ExaminingAction wm = WithResponseSet wm "examiningResponses" (ExaminingResponses wm) => Action wm ('TakesOneOf 'TakesDirectionParameter 'TakesObjectParameter) (ExaminingActionVariables wm)
examiningAction :: ExaminingAction wm
examiningAction = Action
  "examining"
  ["examine", "examining", "look closely at"]
  []
  (ParseArguments (\args -> do
    let examining = case getNoun args of
          DirectionParameter d -> Left d
          ObjectParameter a -> Right a
          _ -> error "impossible"
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
  error ""--unless variables $ sayResponse (#examiningResponses % #examineUndescribedA)

examineDevices :: ExamineRule wm
examineDevices = notImplementedRule "action requires light"

examineSupporters :: ExamineRule wm
examineSupporters = notImplementedRule "action requires light"

examineContainers :: ExamineRule wm
examineContainers = notImplementedRule "action requires light"

examineDirections :: ExamineRule wm
examineDirections = notImplementedRule "action requires light"

standardExamining :: ExamineRule wm
standardExamining = Rule "standard examining rule" forPlayer' $ \Args{..} -> do
  -- if the noun provides the property description and the description of the noun is not "":
  r <- whenRight Nothing (examining variables) $ \obj -> do
    desc <- sayText (view #description obj)
    -- say "[description of the noun][line break]";
    -- now examine text printed is true.
    if (desc /= "") then
       do
        [sayingLn|{desc}|]
        pure $ Just True
      else pure Nothing
  pure (Nothing, Nothing)

reportOtherPeopleExamining :: ExamineRule wm
reportOtherPeopleExamining = error ""
