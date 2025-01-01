{-# LANGUAGE RecordWildCards #-}
module Yaifl.Text.ListWriter.WriteListR
  ( writeListOfThings
  , writeListR

  ) where


import Yaifl.Prelude hiding (asks, Reader, runReader)

import Effectful.Reader.Static
import Effectful.Writer.Static.Local
import Yaifl.Core.Activity
import Yaifl.Core.Metadata

import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.Responses
import Yaifl.Text.Say

import qualified Data.Text as T
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Text.ListWriter.Parameters
import Yaifl.Text.ListWriter.Responses
import Yaifl.Text.ListWriter.AfterEntry


-- so inform has two different ways to write a list - one that is a list of arbitrary items (markedlistiterator)
-- and objecttreeiterator. this seems like it's all a giant pain so...we have a list, let's just print that
-- coalesce the list if this is the first go round
writeListR ::
  forall wm es.
  Writer Text :> es
  => RuleEffects wm es
  => WithListWriting wm
  => Reader (ListWriting wm) :> es
  => State (ListWritingVariables wm) :> es
  => Eff es ()
writeListR = do
  fromStart <- use @(ListWritingVariables wm) #fromStart
  ListWritingParameters{..} <- use @(ListWritingVariables wm) #lwp
  let adjustedList = if fromStart then coalesceList contents else contents
  when prefacingWithIsAre $ do
    a <- getMentionedThing
    case listToMaybe adjustedList of
      Nothing -> pass
      Just (SingleObject o) -> regarding (Just o)
      Just _ -> regardingMany
    sayTellResponse V a
    if withNewlines
    then tell ":\n"
    else tell " "
    modify @(ListWritingVariables wm) (#lwp % #prefacingWithIsAre .~ False)
  --isAre is now off.
  forM_ (zip [1..] adjustedList) $ \(i, item) -> do
    oxfordComma <- use @Metadata #oxfordCommaEnabled
    case item of
      SingleObject obj -> singleClassGroup (i == 1) 1 (SingleObject obj)
      GroupedItems xs -> multiClassGroup xs
      -- because they are identical, we should be able to do everything by
      -- the first element + the length of the list
      EquivalenceClass l@(x :| _) -> singleClassGroup (i == 1) (length l) (SingleObject x)
    -- if we're printing as a sentence
    when asEnglishSentence $ do
      -- and we're onn the second last item and the oxford comma is enabled
      when (i == (length adjustedList - 1) && oxfordComma) $ do
        -- and we have more than 2 items in the list
        when (length adjustedList > 2) $ tell @Text ","
        sayTellResponse C (getObjectOut item)
      when (i < (length adjustedList - 1)) $ tell ", "

writeListOfThings ::
  forall wm es.
  WithListWriting wm
  => RuleEffects wm es
  => Writer Text :> es
  => ListWritingParameters wm
  -> Eff es ()
writeListOfThings lwp = do
  lr <- use @(ResponseCollector wm) (#responseCollection % #listWriterResponses)
  runReader (LW lr) $ evalStateLocal (LWV
    { fromStart = True
    , depth = initialDepth lwp
    , margin = if withExtraIndentation lwp then 1 else 0
    , lwp = lwp
    }) $ do
    case contents lwp of
      [] -> do
        a <- getMentionedThing
        if prefacingWithIsAre lwp
        then sayTellResponse W a
        else sayTellResponse Y a
        when (withNewlines lwp) $ tell "\n"
      _
        | not (asListingActivity lwp) -> writeListR
      _ -> void $ doActivity #listingContents lwp


getObjectOut :: ListWritingItem wm -> Thing wm
getObjectOut = \case
  SingleObject o -> o
  GroupedItems (g :| _) -> getObjectOut g
  EquivalenceClass (g :| _) -> g

singleClassGroup ::
  forall wm es.
  WithListWriting wm
  => Writer Text :> es
  => State (ListWritingVariables wm) :> es
  => Reader (ListWriting wm) :> es
  => RuleEffects wm es
  => Bool
  -> Int -- ^ number of class elements
  -> ListWritingItem wm
  -> Eff es ()
singleClassGroup isFirst cnt item' = do
  LWV{lwp, depth, margin} <- get
  when (indented lwp) $ tell (T.replicate (2 * (depth+margin)) " ")
  let
  let item = getObjectOut item'
  if cnt == 1
    -- TODO: tidy all this up
  then do
    if
      | suppressingAllArticles lwp -> [sayingTell|{item}|]
      | usingDefiniteArticle lwp && isFirst && capitaliseFirstArticle lwp -> [sayingTell|{The item}|]
      | usingDefiniteArticle lwp -> [sayingTell|{the item}|]
      | isFirst && capitaliseFirstArticle lwp -> [sayingTell|{A item}|]
      | otherwise -> [sayingTell|{a item}|]
  else do
    [sayingTell|{cnt} |]
    void $ doActivity #printingANumberOf (cnt, item)
  writeAfterEntry writeListOfThings cnt item


coalesceList :: [ListWritingItem wm] -> [ListWritingItem wm]
coalesceList = id

multiClassGroup ::
  forall wm es.
  WithListWriting wm
  => Writer Text :> es
  => State (ListWritingVariables wm) :> es
  => RuleEffects wm es
  => Reader (ListWriting wm) :> es
  => NonEmpty (ListWritingItem wm)
  -> Eff es ()
multiClassGroup groupOfThings = do
  LWV{lwp, depth, margin} <- get
  when (indented lwp) $ tell (T.replicate (2 * (depth+margin)) " ")
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