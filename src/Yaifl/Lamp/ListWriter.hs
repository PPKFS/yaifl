{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module Yaifl.Lamp.ListWriter
  ( ListWritingParameters(..)
  , withContents
  , blankListWritingParameters

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
import Effectful.Optics
import Yaifl.Core.Metadata
import qualified Data.Text as T

type WithListWriting wm = (
  WithPrintingNameOfSomething wm
  , WithActivity "listingContents" wm (ListWritingParameters wm) ()
  , WithActivity "groupingTogether" wm (AnyObject wm) ()
  )

data ListWritingItem wm =
  SingleObject (AnyObject wm)
  -- a group may contain equivalence classes of objects
  | GroupedItems (NonEmpty (ListWritingItem wm))
  | EquivalenceClass (NonEmpty (AnyObject wm))

instance Display (ListWritingItem wm) where
  displayBuilder = pure ""

data ListWritingParameters wm = ListWritingParameters
  { contents :: [ListWritingItem wm]
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

blankListWritingParameters :: [AnyObject wm] -> ListWritingParameters wm
blankListWritingParameters c = ListWritingParameters
  { contents = map SingleObject c
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

withContents :: [AnyObject wm] -> ListWritingParameters wm
withContents = (#includingContents .~ True) . blankListWritingParameters

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

writeListOfThings ::
  WithListWriting wm
  => RuleEffects wm es
  => Writer Text :> es
  => ListWritingParameters wm
  -> Eff es ()
writeListOfThings lwp = evalStateLocal (LWV
  { fromStart = True
  , depth = 0
  , margin = if withExtraIndentation lwp then 1 else 0
  , lwp = lwp
  }) $ do
  case contents lwp of
    [] -> do
      if prefacingWithIsAre lwp
      then sayListWriterResponse #w ()
      else sayListWriterResponse #y ()
      when (withNewlines lwp) $ tell "\n"
    _
      | not (asListingActivity lwp) -> writeListR
    _ -> void $ doActivity #listingContents lwp

-- so inform has two different ways to write a list - one that is a list of arbitrary items (markedlistiterator)
-- and objecttreeiterator. this seems like it's all a giant pain so...we have a list, let's just print that
-- coalesce the list if this is the first go round
writeListR ::
  forall wm es.
  Writer Text :> es
  => RuleEffects wm es
  => WithListWriting wm
  => State (ListWritingVariables wm) :> es
  => Eff es ()
writeListR = do
  fromStart <- use @(ListWritingVariables wm) #fromStart
  ListWritingParameters{..} <- use @(ListWritingVariables wm) #lwp
  let adjustedList = if fromStart then coalesceList contents else contents
  when prefacingWithIsAre $ do
    sayListWriterResponse #v ()
    if withNewlines
    then tell ":\n"
    else tell " "
  --isAre is now off.
  forM_ (zip [length adjustedList..] adjustedList) $ \(i, item) -> do
    oxfordComma <- use @Metadata #oxfordCommaEnabled

    case item of
      SingleObject obj -> singleClassGroup 1 (SingleObject obj)

      GroupedItems xs -> multiClassGroup xs
      -- because they are identical, we should be able to do everything by
      -- the first element + the length of the list
      EquivalenceClass l@(x :| _) -> singleClassGroup (length l) (SingleObject x)
    -- if we're printing as a sentence
    when asEnglishSentence $ do
      -- and we're onn the second last item and the oxford comma is enabled
      when (i == 1 && oxfordComma) $ do
        -- and we have more than 2 items in the list
        when (length adjustedList > 2) $ tell @Text ","
        sayListWriterResponse #c ()
      when (i > 1) $ tell ", "

singleClassGroup :: Int -> ListWritingItem wm -> Eff es a1
singleClassGroup = error ""

coalesceList :: [ListWritingItem wm] -> [ListWritingItem wm]
coalesceList = id

multiClassGroup ::
  forall wm es.
  WithListWriting wm
  => Writer Text :> es
  => State (ListWritingVariables wm) :> es
  => RuleEffects wm es
  => NonEmpty (ListWritingItem wm)
  -> Eff es ()
multiClassGroup groupOfThings = do
  LWV{lwp, depth, margin} <- get
  when (indented lwp) $ tell (T.replicate (2 * (depth+margin)) " ")
  let getObjectOut = \case
        SingleObject o -> o
        GroupedItems (g :| _) -> getObjectOut g
        EquivalenceClass (g :| _) -> g
  --  as we know the items are grouped, we should be fine to just take the first element as the class representative
  beginActivity #groupingTogether (getObjectOut $ head groupOfThings)
  whenHandling' #groupingTogether $ do
    o <- get @(ListWritingVariables wm)
    modify @(ListWritingVariables wm) (\s -> s &
      #lwp % #contents .~ toList groupOfThings
      & #lwp % #withNewlines .~ False
      & #fromStart .~ False
      & #margin %~ (+1))
    writeListR
    put o
  void $ endActivity #groupingTogether


data ListWriterResponses wm = LWR
  { c :: Response wm ()
  , w :: Response wm ()
  , v :: Response wm ()
  , y :: Response wm ()
  } deriving stock ( Generic )

sayListWriterResponse :: Lens' (ListWriterResponses wm) (Response wm v) -> v -> Eff es ()
sayListWriterResponse = error ""

-- what else...probably want a test *now* for grouping items and for coalescing the list correctly