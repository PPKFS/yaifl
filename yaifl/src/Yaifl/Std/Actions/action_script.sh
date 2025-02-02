#!/bin/sh

#define the template.
cat  << EOF > /home/avery/yaifl/yaifl/src/Yaifl/Std/Actions/$1.hs
module Yaifl.Std.Actions.$1
  ( $1Responses(..)
  , $1Action
  , $1Rule
  , $2Action
  , $2Responses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data $1Responses wm =
  $3FooA

$2Responses :: $1Responses wm -> Response wm (Args wm (Thing wm))
$2Responses = \case
  _ -> notImplementedResponse "response"

type $1Action wm = Action wm ($1Responses wm) 'TakesNoParameter (Thing wm)
type $1Rule wm = ActionRule wm ($1Action wm) (Thing wm)
$2Action :: $1Action wm
$2Action = (makeAction "$2")
  { responses = $2Responses
  , checkRules = makeActionRulebook "check $2" ([] <> map notImplementedRule
    [ "can't do $2"
    ])
  , carryOutRules = makeActionRulebook "carry out $2" [ notImplementedRule "standard $2"  ]
  , reportRules = makeActionRulebook "report $2"  [ notImplementedRule "standard report $2"  ]
  }