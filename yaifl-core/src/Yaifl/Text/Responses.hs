{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Yaifl.Text.Responses
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Response system that enables customisation of text output without modifying game logic.

This module provides a response system where responses act as customisable placeholders.
Game authors can change response text (e.g., modifying the "dark room" description) without
creating duplicate rules or altering the underlying activity logic.

Key components:
- `Response`: Parameterised response type that generates text output
- Response constructors: `notImplementedResponse`, `constResponse` for creating responses
- Response execution: `sayResponse`, `sayTellResponse` for running responses in context
- `WithResponseSet`: Type alias for response set constraints
-}

module Yaifl.Text.Responses
  ( -- * Core Types
    Response(..)
  , WithResponseSet

  -- * Response Constructors
  , constResponse
  , notImplementedResponse

  -- * Response Execution
  , sayResponse
  , sayTellResponse
  )
where

import Yaifl.Prelude hiding (Reader, ask)
import Effectful.Writer.Static.Local (Writer, execWriter)
import Yaifl.WorldModel
import GHC.TypeLits
import Yaifl.Text.SayQQ
import Effectful.Reader.Static
import Yaifl.Effects.RuleEffects
import Yaifl.Text.SayableValue

-- | A parameterised response that generates text output.
--
-- Responses act as customisable placeholders that can be overridden to change
-- text output without modifying the underlying game logic.
newtype Response wm v = Response { runResponse :: forall es. SayableValue (WMText wm) wm => (RuleEffects wm es) => v -> Eff (Writer Text : es) () }

makeFieldLabelsNoPrefix ''Response

-- | Create a response that indicates a feature is not yet implemented.
notImplementedResponse :: Text -> Response wm a
notImplementedResponse t = Response $ const (sayTell t)

-- | Create a constant response that always produces the same text.
constResponse ::
  Text
  -> Response wm a
constResponse t = Response $ const [sayingTell|{t}|]

-- | Execute a response and output the result using 'say'.
--
-- Looks up the response in the response set and executes it with the given variable.
sayResponse ::
  (RuleEffects wm es, SayableValue (WMText wm) wm)
  => Reader a :> es
  => Is k A_Lens
  => LabelOptic' "responses" k a (resp -> Response wm v)
  => resp -- ^ Response selector from the response set
  -> v -- ^ Variable passed to the response
  -> Eff es ()
sayResponse aL v = do
  c <- ask
  let (Response res) = (c ^. castOptic @A_Lens #responses) aL
  r <- execWriter (res v)
  say r

-- | Execute a response and output the result to a Writer effect.
--
-- Unlike 'sayResponse', this writes the response text to a Writer effect
-- where it can be captured as Text rather than immediately printed.
sayTellResponse ::
  (RuleEffects wm es, SayableValue (WMText wm) wm)
  => Reader a :> es
  => Writer Text :> es
  => Is k A_Lens
  => LabelOptic' "responses" k a (resp -> Response wm v)
  => resp -- ^ Response selector from the response set
  -> v -- ^ Variable passed to the response
  -> Eff es ()
sayTellResponse aL v = do
  c <- ask
  let (Response res) = (c ^. castOptic @A_Lens #responses) aL
  r <- execWriter (res v)
  sayTell r

-- | Constraint alias for working with response sets.
--
-- Provides the necessary constraints for accessing response sets stored in the world model.
type WithResponseSet wm k (name :: Symbol) v = (Is k A_Lens, LabelOptic' name k (WMResponses wm) v)
