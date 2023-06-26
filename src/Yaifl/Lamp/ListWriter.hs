{-# LANGUAGE RecordWildCards #-}
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
  } deriving stock ( Generic )

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
  -- ???
  , andOrCapitalised = False
  }

withContents :: [AnyObject wm] -> ListWritingVariables wm
withContents = (#includingContents .~ True) . blankListWritingVariables

-- https://ganelson.github.io/inform/WorldModelKit/S-lst.html#SP16
  -- turns out the "LW_RESPONSE" stuff is all dfefined in Variables and Rulebooks in the standard rules

instance SayableValue (SayArticle "aListWithContents" (ListWritingVariables wm)) wm where
  sayTell (SayArticle _ lwv) =
    writeListOfThings $ lwv
          { includingContents = True
          , tersely = True
          , givingBriefInventoryInformation = True
          , notListingConcealedItems = True
          }

instance SayableValue (SayArticle "isAreThe" (ListWritingVariables wm)) wm where
  sayTell (SayArticle _ lwv) =
    writeListOfThings $ lwv
          { asEnglishSentence = True
          , usingDefiniteArticle = True
          , prefacingWithIsAre = True
          }

instance SayableValue (SayArticle "isAre" (ListWritingVariables wm)) wm where
  sayTell (SayArticle _ lwv) =
    writeListOfThings $ lwv
          { asEnglishSentence = True
          , suppressingAllArticles = True
          , prefacingWithIsAre = True
          }

instance SayableValue (SayArticle "isAreA" (ListWritingVariables wm)) wm where
  sayTell (SayArticle _ lwv) =
    writeListOfThings $ lwv
          { asEnglishSentence = True
          , prefacingWithIsAre = True
          }

instance SayableValue (SayArticle "The" (ListWritingVariables wm)) wm where
  sayTell (SayArticle _ lwv) =
    writeListOfThings $ lwv
          { asEnglishSentence = True
          , capitaliseFirstArticle = True
          , usingDefiniteArticle = True
          }

instance SayableValue (SayArticle "the" (ListWritingVariables wm)) wm where
  sayTell (SayArticle _ lwv) =
    writeListOfThings $ lwv
          { asEnglishSentence = True
          , usingDefiniteArticle = True
          }

instance SayableValue (SayArticle "A" (ListWritingVariables wm)) wm where
  sayTell (SayArticle _ lwv) =
    writeListOfThings $ lwv
          { asEnglishSentence = True
          , capitaliseFirstArticle = True
          }

instance SayableValue (SayArticle "a" (ListWritingVariables wm)) wm where
  sayTell (SayArticle _ lwv) =
    writeListOfThings $ lwv
          { asEnglishSentence = True
          }

instance SayableValue (ListWritingVariables wm) wm where
  sayTell lwv =
    writeListOfThings $ lwv
          { asEnglishSentence = True
          , suppressingAllArticles = True
          }

writeListOfThings :: ListWritingVariables wm -> Eff es ()
writeListOfThings ListWritingVariables{..} = do
  let margin = if withExtraIndentation then 1 else 0
  -- if the list is empty
    -- and we have `isare`, response w
    -- otherwise response y
    -- if newline, then print newline
  -- otherwise if we are not using the activity we do writelistr
  -- otherwise we carry out the listing contents activity
  pass
