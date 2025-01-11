module MessageLog where

import Yaifl.Prelude
import Yaifl.Text.Print
import Prettyprinter
import Rogue.Rendering.Viewport

data Message = Message
  { doc :: SimpleDocStream MessageAnnotation
  , height :: Int
  }

data MessageLog layers = MessageLog
  { viewport :: Viewport layers
  , log :: [Message]
  --, scrollbar :: Maybe Scrollbar
  } deriving stock (Generic)

addMessage :: SimpleDocStream MessageAnnotation -> MessageLog layers -> MessageLog layers
addMessage doc ml = let msg = makeMessage (view #viewport ml) doc in ml & log %~ (msg:)