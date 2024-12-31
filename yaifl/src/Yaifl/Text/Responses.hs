{-# LANGUAGE UndecidableInstances #-}


module Yaifl.Text.Responses
  ( Response(..)
  , constResponse
  , notImplementedResponse
  , sayResponse
  , sayTellResponse
  , WithResponseSet
  ) where

import Yaifl.Prelude hiding (Reader, ask)
import Effectful.Writer.Static.Local (Writer, execWriter)
import Yaifl.Core.WorldModel
import GHC.TypeLits
import Yaifl.Model.Rules.RuleEffects
import Yaifl.Text.SayQQ
import Effectful.Reader.Static

newtype Response wm v = Response { runResponse :: forall es. SayableValue (WMText wm) wm => (RuleEffects wm es) => v -> Eff (Writer Text : es) () }

makeFieldLabelsNoPrefix ''Response

notImplementedResponse :: Text -> Response wm a
notImplementedResponse t = Response $ const (sayTell t)

constResponse ::
  Text
  -> Response wm a
constResponse t = Response $ const [sayingTell|{t}|]

sayResponse ::
  (RuleEffects wm es, SayableValue (WMText wm) wm)
  => Reader a :> es
  => Is k A_Lens
  => LabelOptic' "responses" k a (resp -> Response wm v)
  => resp
  -> v
  -> Eff es ()
sayResponse aL v = do
  c <- ask
  let (Response res) = (c ^. castOptic @A_Lens #responses) aL
  r <- execWriter (res v)
  say r

sayTellResponse ::
  (RuleEffects wm es, SayableValue (WMText wm) wm)
  => Reader a :> es
  => Writer Text :> es
  => Is k A_Lens
  => LabelOptic' "responses" k a (resp -> Response wm v)
  => resp
  -> v
  -> Eff es ()
sayTellResponse aL v = do
  c <- ask
  let (Response res) = (c ^. castOptic @A_Lens #responses) aL
  r <- execWriter (res v)
  sayTell r

type WithResponseSet wm k (name :: Symbol) v = (Is k A_Lens, LabelOptic' name k (WMResponses wm) v)
