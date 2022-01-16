module Yaifl.Activities.Common
(
  doActivity
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
    blankRulebook n' = Rulebook n' Nothing (ParseArguments (return . Just)) []


doActivity
  :: MonadWorld s m
  => (ActivityCollection s -> Activity s v r)
  -> v
  -> m (Maybe r)
doActivity l c = do
  ac <- gets $ l . _activities
  x <- runRulebookAndReturnVariables (_activityBeforeRules ac) c
  mr <- runRulebookAndReturnVariables (_activityCarryOutRules ac) (maybe c fst x)
  _ <- runRulebookAndReturnVariables (_activityAfterRules ac) (maybe c fst mr)
  return $ snd =<< mr
