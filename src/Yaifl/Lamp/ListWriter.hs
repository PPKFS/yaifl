{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module Yaifl.Lamp.ListWriter
  ( ListWritingVariables(..)
  , withContents
  , blankListWritingVariables

  ) where

import Yaifl.Core.Object
import Solitude
import Yaifl.Core.Rules.RuleEffects
--import Effectful.Writer.Static.Local ( tell )
import Yaifl.Lamp.Say
import Yaifl.Core.Actions.Activity
import Data.Text.Display
import Yaifl.Core.Responses
import Effectful.Writer.Static.Local

type WithListWriting wm = (
  WithPrintingNameOfSomething wm
  , WithActivity "listingContents" wm (ListWritingVariables wm) ()
  )

data ListWritingVariables wm = ListWritingVariables
  { contents :: [AnyObject wm]
  -- New-line after each entry NEWLINE_BIT
  , withNewlines :: Bool
  -- Indent each entry by depth INDENT_BIT
  , indented :: Bool
  -- Full inventory information after entry FULLINV_BIT
  , givingInventoryInformation :: Bool
  -- English sentence style, with commas and and ENGLISH_BIT
  , asEnglishSentence :: Bool
  -- Recurse downwards with usual rules RECURSE_BIT
  , includingContents :: Bool
  -- Always recurse downwards ALWAYS_BIT
  , includingAllContents :: Bool
  -- More terse English style TERSE_BIT
  , tersely :: Bool
  , asListingActivity :: Bool
  -- Only brief inventory information after entry PARTINV_BIT
  , givingBriefInventoryInformation :: Bool
  -- we ignore WORKFLAG_BIT
  , usingDefiniteArticle :: Bool
  -- Print " is" or " are" before list ISARE_BIT
  , prefacingWithIsAre :: Bool
  -- Omit objects with "concealed" or "scenery": CONCEAL_BIT
  , notListingConcealedItems :: Bool
  -- Print no articles, definite or not NOARTICLE_BIT
  , suppressingAllArticles :: Bool
  -- New in I7: extra indentation of 1 level EXTRAINDENT_BIT
  , withExtraIndentation :: Bool
  -- Capitalise first article in list CFIRSTART_BIT
  , capitaliseFirstArticle :: Bool
  -- ???
  , andOrCapitalised :: Bool
  , fromStart :: Bool
  } deriving stock ( Generic )

instance Display (ListWritingVariables wm) where
  displayBuilder = pure ""

blankListWritingVariables :: [AnyObject wm] -> ListWritingVariables wm
blankListWritingVariables c = ListWritingVariables
  { contents = c
  , withNewlines = False
  , indented = False
  , givingInventoryInformation = False
  , asEnglishSentence = True
  , includingContents = False
  , includingAllContents = False
  , tersely = False
  , givingBriefInventoryInformation = False
  , usingDefiniteArticle = False
  , prefacingWithIsAre = False
  , notListingConcealedItems = False
  , suppressingAllArticles = False
  , withExtraIndentation = False
  , capitaliseFirstArticle = False
  , asListingActivity = True
  , fromStart = True
  -- ???
  , andOrCapitalised = False
  }

withContents :: [AnyObject wm] -> ListWritingVariables wm
withContents = (#includingContents .~ True) . blankListWritingVariables

-- https://ganelson.github.io/inform/WorldModelKit/S-lst.html#SP16
  -- turns out the "LW_RESPONSE" stuff is all dfefined in Variables and Rulebooks in the standard rules

instance WithListWriting wm => SayableValue (SayArticle "aListWithContents" (ListWritingVariables wm)) wm where
  sayTell (SayArticle _ lwv) =
    writeListOfThings $ lwv
          { includingContents = True
          , tersely = True
          , givingBriefInventoryInformation = True
          , notListingConcealedItems = True
          }

instance WithListWriting wm => SayableValue (SayArticle "isAreThe" (ListWritingVariables wm)) wm where
  sayTell (SayArticle _ lwv) =
    writeListOfThings $ lwv
          { asEnglishSentence = True
          , usingDefiniteArticle = True
          , prefacingWithIsAre = True
          }

instance WithListWriting wm => SayableValue (SayArticle "isAre" (ListWritingVariables wm)) wm where
  sayTell (SayArticle _ lwv) =
    writeListOfThings $ lwv
          { asEnglishSentence = True
          , suppressingAllArticles = True
          , prefacingWithIsAre = True
          }

instance WithListWriting wm => SayableValue (SayArticle "isAreA" (ListWritingVariables wm)) wm where
  sayTell (SayArticle _ lwv) =
    writeListOfThings $ lwv
          { asEnglishSentence = True
          , prefacingWithIsAre = True
          }

instance WithListWriting wm => SayableValue (SayArticle "The" (ListWritingVariables wm)) wm where
  sayTell (SayArticle _ lwv) =
    writeListOfThings $ lwv
          { asEnglishSentence = True
          , capitaliseFirstArticle = True
          , usingDefiniteArticle = True
          }

instance WithListWriting wm => SayableValue (SayArticle "the" (ListWritingVariables wm)) wm where
  sayTell (SayArticle _ lwv) =
    writeListOfThings $ lwv
          { asEnglishSentence = True
          , usingDefiniteArticle = True
          }

instance WithListWriting wm => SayableValue (SayArticle "A" (ListWritingVariables wm)) wm where
  sayTell (SayArticle _ lwv) =
    writeListOfThings $ lwv
          { asEnglishSentence = True
          , capitaliseFirstArticle = True
          }

instance WithListWriting wm => SayableValue (SayArticle "a" (ListWritingVariables wm)) wm where
  sayTell (SayArticle _ lwv) =
    writeListOfThings $ lwv
          { asEnglishSentence = True
          }

instance WithListWriting wm => SayableValue (ListWritingVariables wm) wm where
  sayTell lwv =
    writeListOfThings $ lwv
          { asEnglishSentence = True
          , suppressingAllArticles = True
          }

writeListOfThings ::
  WithListWriting wm
  => RuleEffects wm es
  => Writer Text :> es
  => ListWritingVariables wm
  -> Eff es ()
writeListOfThings lwv = do
  let margin = if withExtraIndentation lwv then 1 else 0
  case contents lwv of
    [] -> do
      if prefacingWithIsAre lwv
      then sayListWriterResponse #w ()
      else sayListWriterResponse #y ()
      when (withNewlines lwv) $ tell "\n"
    _
      | not (asListingActivity lwv) -> writeListR lwv
    _ -> void $ doActivity #listingContents lwv

-- so inform has two different ways to write a list - one that is a list of arbitrary items (markedlistiterator)
-- and objecttreeiterator. this seems like it's all a giant pain so...we have a list, let's just print that
writeListR :: ListWritingVariables wm -> Eff es ()
writeListR = error ""

data ListWriterResponses wm = LWR
  { w :: Response wm ()
  , y :: Response wm ()
  } deriving stock ( Generic )

sayListWriterResponse :: Lens' (ListWriterResponses wm) (Response wm v) -> v -> Eff es ()
sayListWriterResponse = error ""

-- what else...probably want a test *now* for grouping items and for coalescing the list correctly