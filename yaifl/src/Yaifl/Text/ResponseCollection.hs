
module Yaifl.Text.ResponseCollection where

import Solitude
import Yaifl.Text.ListWriter
import Yaifl.Text.Responses

data ResponseCollection wm = ResponseCollection
  { listWriterResponses :: ListWriterResponses -> Response wm ()
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ResponseCollection

blankResponseCollection :: ResponseCollection wm
blankResponseCollection = ResponseCollection
  { listWriterResponses = listWriterResponsesImpl
  }