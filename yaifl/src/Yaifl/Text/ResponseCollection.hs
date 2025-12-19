
module Yaifl.Text.ResponseCollection where

import Yaifl.Prelude
import Yaifl.Text.Responses
import Yaifl.Thing.Kind
import Yaifl.Text.ListWriter.Responses

data ResponseCollection wm = ResponseCollection
  { listWriterResponses :: ListWriterResponses -> Response wm (Thing wm)
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ResponseCollection

blankResponseCollection :: ResponseCollection wm
blankResponseCollection = ResponseCollection
  { listWriterResponses = listWriterResponsesImpl
  }