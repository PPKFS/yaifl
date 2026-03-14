{-# LANGUAGE UndecidableInstances #-}

module Yaifl.ActionCollection
  ( ActionCollection(..)
  , blankActionCollection
  ) where

import Yaifl.Action
import Yaifl.Actions.GoesWith
import Yaifl.Prelude
import Yaifl.Actions.Answering
import Yaifl.Actions.Asking
import Yaifl.Actions.AskingFor
import Yaifl.Actions.Attacking
import Yaifl.Actions.Burning
import Yaifl.Actions.Buying
import Yaifl.Actions.Climbing
import Yaifl.Actions.Closing
import Yaifl.Actions.Consulting
import Yaifl.Actions.Cutting
import Yaifl.Actions.Drinking
import Yaifl.Actions.Dropping
import Yaifl.Actions.Eating
import Yaifl.Actions.Entering
import Yaifl.Actions.Examining
import Yaifl.Actions.Exiting
import Yaifl.Actions.GettingOff
import Yaifl.Actions.Giving
import Yaifl.Actions.Going
import Yaifl.Actions.Inserting
import Yaifl.Actions.Jumping
import Yaifl.Actions.Kissing
import Yaifl.Actions.Listening
import Yaifl.Actions.Locking
import Yaifl.Actions.Looking
import Yaifl.Visibility
import Yaifl.Actions.LookingUnder
import Yaifl.Actions.Opening
import Yaifl.Actions.Pulling
import Yaifl.Actions.Pushing
import Yaifl.Actions.PuttingOn
import Yaifl.Actions.Removing
import Yaifl.Actions.Rubbing
import Yaifl.Actions.SayingNo
import Yaifl.Actions.SayingSorry
import Yaifl.Actions.SayingYes
import Yaifl.Actions.Searching
import Yaifl.Actions.Setting
import Yaifl.Actions.Showing
import Yaifl.Actions.Sleeping
import Yaifl.Actions.Smelling
import Yaifl.Actions.Squeezing
import Yaifl.Actions.Swinging
import Yaifl.Actions.SwitchingOff
import Yaifl.Actions.SwitchingOn
import Yaifl.Actions.Taking
import Yaifl.Actions.TakingInventory
import Yaifl.Actions.TakingOff
import Yaifl.Actions.Tasting
import Yaifl.Actions.Telling
import Yaifl.Actions.Thinking
import Yaifl.Actions.Throwing
import Yaifl.Actions.Touching
import Yaifl.Actions.Turning
import Yaifl.Actions.Tying
import Yaifl.Actions.Unlocking
import Yaifl.Actions.Waiting
import Yaifl.Actions.Waking
import Yaifl.Actions.WakingUp
import Yaifl.Actions.Waving
import Yaifl.Actions.WavingHands
import Yaifl.Actions.Wearing
import Yaifl.Properties

-- | The standard actions before they are existentially wrapped. This is so we can modify them during
-- world construction as we lose the type information later and cannot modify a `WrappedAction`.
data ActionCollection wm = ActionCollection
  { going :: Action wm GoingResponses ('Optionally ('TakesOneOf 'TakesDirectionParameter 'TakesObjectParameter)) (GoingActionVariables wm)
  , answering :: AnsweringAction wm
  , asking :: AskingAction wm
  , askingFor :: AskingForAction wm
  , attacking :: AttackingAction wm
  , burning :: BurningAction wm
  , buying :: BuyingAction wm
  , climbing :: ClimbingAction wm
  , closing :: ClosingAction wm
  , consulting :: ConsultingAction wm
  , cutting :: CuttingAction wm
  , drinking :: DrinkingAction wm
  , dropping :: DroppingAction wm
  , eating :: EatingAction wm
  , entering :: EnteringAction wm
  , examining :: ExaminingAction wm
  , exiting :: ExitingAction wm
  , gettingOff :: GettingOffAction wm
  , giving :: GivingAction wm
  , inserting :: InsertingAction wm
  , jumping :: JumpingAction wm
  , kissing :: KissingAction wm
  , listening :: ListeningAction wm
  , locking :: LockingAction wm
  , looking :: Action wm (LookingResponses wm) ('Optionally 'TakesConstantParameter) (LookingActionVariables wm)
  , lookingUnder:: LookingUnderAction wm
  , opening :: OpeningAction wm
  , pulling :: PullingAction wm
  , pushing :: PushingAction wm
  , puttingOn:: PuttingOnAction wm
  , removing :: RemovingAction wm
  , rubbing :: RubbingAction wm
  , sayingNo:: SayingNoAction wm
  , sayingSorry:: SayingSorryAction wm
  , sayingYes:: SayingYesAction wm
  , searching :: SearchingAction wm
  , setting :: SettingAction wm
  , showing :: ShowingAction wm
  , sleeping :: SleepingAction wm
  , smelling :: SmellingAction wm
  , squeezing :: SqueezingAction wm
  , swinging :: SwingingAction wm
  , switchingOff:: SwitchingOffAction wm
  , switchingOn :: SwitchingOnAction wm
  , taking :: TakingAction wm
  , takingInventory:: TakingInventoryAction wm
  , takingOff:: TakingOffAction wm
  , tasting :: TastingAction wm
  , telling :: TellingAction wm
  , thinking :: ThinkingAction wm
  , throwing :: ThrowingAction wm
  , touching :: TouchingAction wm
  , turning :: TurningAction wm
  , tying :: TyingAction wm
  , unlocking :: UnlockingAction wm
  , waiting :: WaitingAction wm
  , waking :: WakingAction wm
  , wakingUp:: WakingUpAction wm
  , waving :: WavingAction wm
  , wavingHands:: WavingHandsAction wm
  , wearing :: WearingAction wm
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ActionCollection

blankActionCollection ::
  HasStandardProperties wm
  => ActionCollection wm
blankActionCollection = ActionCollection
  { going = goingAction
  , answering = answeringAction
  , asking = askingAction
  , askingFor = askingForAction
  , attacking = attackingAction
  , burning = burningAction
  , buying = buyingAction
  , climbing = climbingAction
  , closing = closingAction
  , consulting = consultingAction
  , cutting = cuttingAction
  , drinking = drinkingAction
  , dropping = droppingAction
  , eating = eatingAction
  , entering = enteringAction
  , examining = examiningAction
  , exiting = exitingAction
  , gettingOff = gettingOffAction
  , giving = givingAction
  , inserting = insertingAction
  , jumping = jumpingAction
  , kissing = kissingAction
  , listening = listeningAction
  , locking = lockingAction
  , looking = lookingAction
  , lookingUnder= lookingUnderAction
  , opening = openingAction
  , pulling = pullingAction
  , pushing = pushingAction
  , puttingOn= puttingOnAction
  , removing = removingAction
  , rubbing = rubbingAction
  , sayingNo= sayingNoAction
  , sayingSorry= sayingSorryAction
  , sayingYes= sayingYesAction
  , searching = searchingAction
  , setting = settingAction
  , showing = showingAction
  , sleeping = sleepingAction
  , smelling = smellingAction
  , squeezing = squeezingAction
  , swinging = swingingAction
  , switchingOn = switchingOnAction
  , switchingOff= switchingOffAction
  , taking = takingAction
  , takingInventory = takingInventoryAction
  , takingOff= takingOffAction
  , tasting = tastingAction
  , telling = tellingAction
  , thinking = thinkingAction
  , throwing = throwingAction
  , touching = touchingAction
  , turning = turningAction
  , tying = tyingAction
  , unlocking = unlockingAction
  , waiting = waitingAction
  , waking = wakingAction
  , wakingUp= wakingUpAction
  , waving = wavingAction
  , wavingHands = wavingHandsAction
  , wearing = wearingAction
  }