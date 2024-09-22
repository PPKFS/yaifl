
module Yaifl.Text.ResponseCollection where

import Yaifl.Prelude
import Yaifl.Text.ListWriter
import Yaifl.Text.Responses
import Yaifl.Model.Kinds.Thing

data ResponseCollection wm = ResponseCollection
  { listWriterResponses :: ListWriterResponses -> Response wm (Thing wm)
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ResponseCollection

blankResponseCollection :: ResponseCollection wm
blankResponseCollection = ResponseCollection
  { listWriterResponses = listWriterResponsesImpl
  }