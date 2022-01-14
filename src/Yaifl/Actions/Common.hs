module Yaifl.Actions.Common
( makeActionRulebook

) where

import Yaifl.Prelude
import Yaifl.Common

makeActionRulebook
  :: Text
  -> [Rule o (Args o v) Bool]
  -> ActionRulebook o v
makeActionRulebook n r = ActionRulebook $ Rulebook n Nothing (ParseArguments $ const . Just) r