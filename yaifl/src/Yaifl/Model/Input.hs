module Yaifl.Model.Input
  ( Input(..)
  , waitForInput
  , runInputAsBuffer
  , setInputBuffer
  ) where

import Yaifl.Prelude
import Yaifl.Model.Metadata
import Effectful.Optics
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH ( makeEffect )

data Input :: Effect where
  WaitForInput :: Input m Text

makeEffect ''Input

runInputAsStdin ::
  Eff (Input : es) a
  -> Eff es a
runInputAsStdin = error "not implemented"

runInputAsBuffer ::
  State Metadata :> es
  => Eff (Input : es) a
  -> Eff es a
runInputAsBuffer = interpret $ \_ -> \case
  WaitForInput -> do
    buf <- use #bufferedInput
    case buf of
      [] -> pure "ran out of buffered input"
      (x:xs) -> do
        #bufferedInput .= xs
        pure x

setInputBuffer ::
  State Metadata :> es
  => [Text]
  -> Eff es ()
setInputBuffer b = #bufferedInput .= b