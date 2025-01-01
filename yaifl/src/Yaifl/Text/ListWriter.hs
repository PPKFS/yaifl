{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Yaifl.Text.ListWriter
  ( WithListWriting
  , ListWritingParameters(..)
  , withContents
  , blankListWritingParameters
  , writeListOfThings
  ) where

import Yaifl.Prelude hiding (asks, Reader, runReader)


import Yaifl.Text.Say

import Yaifl.Text.ListWriter.Parameters
import Yaifl.Text.ListWriter.WriteListR


-- https://ganelson.github.io/inform/WorldModelKit/S-lst.html#SP16
  -- turns out the "LW_RESPONSE" stuff is all dfefined in Variables and Rulebooks in the standard rules

instance WithListWriting wm => SayableValue (SayArticle "aListWithContents" (ListWritingParameters wm)) wm where
  sayTell (SayArticle _ lwp) =
    writeListOfThings $ lwp
          { includingContents = True
          , tersely = True
          , givingBriefInventoryInformation = True
          , notListingConcealedItems = True
          }

instance WithListWriting wm => SayableValue (SayArticle "isAreThe" (ListWritingParameters wm)) wm where
  sayTell (SayArticle _ lwp) =
    writeListOfThings $ lwp
          { asEnglishSentence = True
          , usingDefiniteArticle = True
          , prefacingWithIsAre = True
          }

instance WithListWriting wm => SayableValue (SayArticle "isAre" (ListWritingParameters wm)) wm where
  sayTell (SayArticle _ lwp) =
    writeListOfThings $ lwp
          { asEnglishSentence = True
          , suppressingAllArticles = True
          , prefacingWithIsAre = True
          }

instance WithListWriting wm => SayableValue (SayArticle "isAreA" (ListWritingParameters wm)) wm where
  sayTell (SayArticle _ lwp) =
    writeListOfThings $ lwp
          { asEnglishSentence = True
          , prefacingWithIsAre = True
          }

instance WithListWriting wm => SayableValue (SayArticle "The" (ListWritingParameters wm)) wm where
  sayTell (SayArticle _ lwp) =
    writeListOfThings $ lwp
          { asEnglishSentence = True
          , capitaliseFirstArticle = True
          , usingDefiniteArticle = True
          }

instance WithListWriting wm => SayableValue (SayArticle "the" (ListWritingParameters wm)) wm where
  sayTell (SayArticle _ lwp) =
    writeListOfThings $ lwp
          { asEnglishSentence = True
          , usingDefiniteArticle = True
          }

instance WithListWriting wm => SayableValue (SayArticle "A" (ListWritingParameters wm)) wm where
  sayTell (SayArticle _ lwp) =
    writeListOfThings $ lwp
          { asEnglishSentence = True
          , capitaliseFirstArticle = True
          }

instance WithListWriting wm => SayableValue (SayArticle "a" (ListWritingParameters wm)) wm where
  sayTell (SayArticle _ lwp) =
    writeListOfThings $ lwp
          { asEnglishSentence = True
          }

instance WithListWriting wm => SayableValue (ListWritingParameters wm) wm where
  sayTell lwp =
    writeListOfThings $ lwp
          { asEnglishSentence = True
          , suppressingAllArticles = True
          }

-- what else...probably want a test *now* for grouping items and for coalescing the list correctly