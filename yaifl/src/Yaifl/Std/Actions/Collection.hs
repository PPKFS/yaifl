{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Std.Actions.Collection
  ( ActionCollection(..)
  , blankActionCollection
  ) where

import Yaifl.Core.Action
import Yaifl.Core.Actions.GoesWith
import Yaifl.Prelude
import Yaifl.Std.Actions.Answering
import Yaifl.Std.Actions.Asking
import Yaifl.Std.Actions.AskingFor
import Yaifl.Std.Actions.Attacking
import Yaifl.Std.Actions.Burning
import Yaifl.Std.Actions.Buying
import Yaifl.Std.Actions.Climbing
import Yaifl.Std.Actions.Closing
import Yaifl.Std.Actions.Consulting
import Yaifl.Std.Actions.Cutting
import Yaifl.Std.Actions.Drinking
import Yaifl.Std.Actions.Dropping
import Yaifl.Std.Actions.Eating
import Yaifl.Std.Actions.Entering
import Yaifl.Std.Actions.Examining
import Yaifl.Std.Actions.Exiting
import Yaifl.Std.Actions.GettingOff
import Yaifl.Std.Actions.Giving
import Yaifl.Std.Actions.Going
import Yaifl.Std.Actions.Inserting
import Yaifl.Std.Actions.Jumping
import Yaifl.Std.Actions.Kissing
import Yaifl.Std.Actions.Listening
import Yaifl.Std.Actions.Locking
import Yaifl.Std.Actions.Looking
import Yaifl.Std.Actions.Looking.Visibility
import Yaifl.Std.Actions.LookingUnder
import Yaifl.Std.Actions.Opening
import Yaifl.Std.Actions.Pulling
import Yaifl.Std.Actions.Pushing
import Yaifl.Std.Actions.PuttingOn
import Yaifl.Std.Actions.Removing
import Yaifl.Std.Actions.Rubbing
import Yaifl.Std.Actions.SayingNo
import Yaifl.Std.Actions.SayingSorry
import Yaifl.Std.Actions.SayingYes
import Yaifl.Std.Actions.Searching
import Yaifl.Std.Actions.Setting
import Yaifl.Std.Actions.Showing
import Yaifl.Std.Actions.Sleeping
import Yaifl.Std.Actions.Smelling
import Yaifl.Std.Actions.Squeezing
import Yaifl.Std.Actions.Swinging
import Yaifl.Std.Actions.SwitchingOff
import Yaifl.Std.Actions.SwitchingOn
import Yaifl.Std.Actions.Taking
import Yaifl.Std.Actions.TakingInventory
import Yaifl.Std.Actions.TakingOff
import Yaifl.Std.Actions.Tasting
import Yaifl.Std.Actions.Telling
import Yaifl.Std.Actions.Thinking
import Yaifl.Std.Actions.Throwing
import Yaifl.Std.Actions.Touching
import Yaifl.Std.Actions.Turning
import Yaifl.Std.Actions.Tying
import Yaifl.Std.Actions.Unlocking
import Yaifl.Std.Actions.Waiting
import Yaifl.Std.Actions.Waking
import Yaifl.Std.Actions.WakingUp
import Yaifl.Std.Actions.Waving
import Yaifl.Std.Actions.WavingHands
import Yaifl.Std.Actions.Wearing
import Yaifl.Std.Properties

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
  , takingInventory= takingInventoryAction
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
  , wavingHands= wavingHandsAction
  , wearing = wearingAction
  }