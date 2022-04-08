module Yaifl.Activities.Common
(
  doActivity
, makeActivity
) where

import Yaifl.Common
import Yaifl.Rulebooks
import Yaifl.Properties

makeActivity
  :: Text
  -> Rule s v r
  -> Activity s v r
makeActivity n r = Activity n Nothing
  (blankRulebook ("Before " <> n))
  ((blankRulebook ("Carry Out " <> n)) { _rbRules = [r]})
  (blankRulebook ("After " <> n))

TODO: extract
doActivity
  :: MonadWorld s m
  => (ActivityCollection s -> Activity s v r)
  -> v
  -> m (Maybe r)
doActivity l c = withoutMissingObjects (do
  ac <- gets $ l . _activities
  x <- runRulebookAndReturnVariables (_activityBeforeRules ac) c
  mr <- runRulebookAndReturnVariables (_activityCarryOutRules ac) (maybe c fst x)
  _ <- runRulebookAndReturnVariables (_activityAfterRules ac) (maybe c fst mr)
  return $ snd =<< mr) (handleMissingObject "" Nothing)
  
