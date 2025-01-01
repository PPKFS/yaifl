module Yaifl.Text.ListWriter.Responses
  ( ListWriterResponses(..)
  , listWriterResponsesImpl
  , ListWriting(..)
  ) where


import Yaifl.Prelude hiding (asks, Reader, runReader)

import Yaifl.Core.Metadata

import Yaifl.Text.Responses
import Yaifl.Text.Say

import Yaifl.Core.Kinds.Thing
import Yaifl.Core.ObjectLike



newtype ListWriting wm = LW { responses :: ListWriterResponses -> Response wm (Thing wm) } deriving stock (Generic)

data ListWriterResponses = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | Y
  deriving stock (Show, Eq, Ord, Enum, Generic, Bounded, Read)

listWriterResponsesImpl :: ListWriterResponses -> Response wm (Thing wm)
listWriterResponsesImpl = \case
  A -> constResponse " ("
  B -> constResponse ")"
  C -> constResponse " and "
  D -> constResponse "providing light"
  E -> constResponse "closed"
  F -> constResponse "empty"
  G -> constResponse "closed and empty"
  H -> constResponse "closed and providing light"
  I -> constResponse "empty and providing light"
  J -> Response $ const $ do
    oc <- use @Metadata #oxfordCommaEnabled
    [sayingTell|closed, empty{?if oc},{?end if}|]
  R -> Response $ \o -> do
    p <- objectIsKind "person" o
    [sayingTell|on {?if p}whom{?else}which{?end if} |]
  T -> Response $ \o -> do
    p <- objectIsKind "person" o
    [sayingTell|in {?if p}whom{?else}which{?end if} |]
  -- "[regarding list writer internals][are] nothing" (W) TODO
  W -> constResponse "is nothing"
  -- "[regarding list writer internals][are]" (V) TODO
  V -> Response $ \_o -> do
    [sayingTell|#{are}|]
  Y -> constResponse "nothing"
  x -> constResponse $ "need to do response " <> show x <> " "