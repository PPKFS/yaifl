module Yaifl.Activities.Common
(
  doActivity'
, makeActivity
) where

import Yaifl.Common
import Yaifl.Prelude
import Yaifl.Rulebooks

makeActivity
  :: Text
  -> Rule s v r
  -> Activity s v r
makeActivity n r = Activity n Nothing
  (blankRulebook ("Before " <> n))
  ((blankRulebook ("Carry Out " <> n)) { _rbRules = [r]})
  (blankRulebook ("After " <> n))
  where
    blankRulebook n' = Rulebook n' Nothing (\x _ -> Just x) []

doActivity'
  :: (ActivityCollection s -> Activity s v r)
  -> v
  -> World s
  -> (Maybe r, World s)
doActivity' l c = runState (doActivity l c)

doActivity
  :: (ActivityCollection s -> Activity s v r)
  -> v
  -> State (World s) (Maybe r)
doActivity l c = do
  ac <- gets $ l . _activities
  x <- state $ runRulebookAndReturnVariables (_activityBeforeRules ac) c
  mr <- state $ runRulebookAndReturnVariables (_activityCarryOutRules ac) (maybe c fst x)
  _ <- state $ runRulebookAndReturnVariables (_activityAfterRules ac) (maybe c fst mr)
  return $ snd =<< mr
