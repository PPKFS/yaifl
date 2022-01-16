module Yaifl.Actions.Common
( makeActionRulebook

) where

import Yaifl.Prelude
import Yaifl.Common

makeActionRulebook
  :: Text
  -> [Rule o (Args o v) Bool]
  -> ActionRulebook o v
makeActionRulebook n = Rulebook n Nothing (ParseArguments $ \x -> return $ Just x)