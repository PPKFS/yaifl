module Yaifl.Text.ListWriter.Parameters
  ( ListWritingParameters(..)
  , withContents
  , blankListWritingParameters
  , ListWritingVariables(..)
  , ListWritingItem(..)
  , WithListWriting

  ) where

import Yaifl.Prelude hiding (asks, Reader, runReader)
import Yaifl.Std.Activities.PrintingInventoryDetails
import Yaifl.Std.Activities.PrintingRoomDescriptionDetails (WithPrintingRoomDescriptionDetails)
import Yaifl.Core.Actions.Args
import Yaifl.Core.Activity
import Yaifl.Std.Kinds.Container
import Yaifl.Std.Kinds.Supporter
import Yaifl.Core.WorldModel

import Yaifl.Text.Responses
import Yaifl.Text.Say

import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Refreshable
import Yaifl.Text.ListWriter.Responses


type WithListWriting wm = (
  WithPrintingNameOfSomething wm
  , WithActivity "listingContents" wm () (ListWritingParameters wm) ()
  , WithActivity "groupingTogether" wm () (Thing wm) ()
  , WithActivity "printingANumberOf" wm () (Int, Thing wm) ()
  , WithResponseSet wm An_Iso "listWriterResponses" (ListWriterResponses -> Response wm (Thing wm))
  , WithPrintingRoomDescriptionDetails wm
  , WithPrintingInventoryDetails wm
  , WMWithProperty wm Container
  , WMWithProperty wm Supporter
  )

data ListWritingItem wm =
  SingleObject (Thing wm)
  -- a group may contain equivalence classes of objects
  | GroupedItems (NonEmpty (ListWritingItem wm))
  | EquivalenceClass (NonEmpty (Thing wm))

instance Display (WMText wm) => Display (ListWritingItem wm) where
  displayBuilder (SingleObject o) = displayBuilder o
  displayBuilder _ = "some list items"

instance Refreshable wm (ListWritingItem wm) where
  refresh = \case
    SingleObject t -> SingleObject <$> refreshThing t
    GroupedItems ls -> GroupedItems <$> mapM refresh ls
    EquivalenceClass eq -> EquivalenceClass <$> mapM refreshThing eq

data ListWritingParameters (wm :: WorldModel) = ListWritingParameters
  { contents :: [ListWritingItem wm]
  , initialDepth :: Int
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

data ListWritingVariables wm = LWV
  { lwp :: ListWritingParameters wm
  , fromStart :: Bool
  , depth :: Int
  , margin :: Int
  } deriving stock ( Generic )

instance Display (ListWritingParameters wm) where
  displayBuilder = pure ""

instance Display (ListWritingVariables wm) where
  displayBuilder = pure ""

instance Refreshable wm (ListWritingParameters wm) where
  refresh lwp = do
    contents <- mapM refresh (contents lwp)
    return $ lwp { contents }

blankListWritingParameters :: [Thing wm] -> ListWritingParameters wm
blankListWritingParameters c = ListWritingParameters
  { -- TODO: this really should have any sort of grouping...
    contents = map SingleObject c
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
  , initialDepth = 0
  }

withContents :: [Thing wm] -> ListWritingParameters wm
withContents = (#includingContents .~ True) . blankListWritingParameters
