{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module Yaifl.Text.ListWriter
  ( ListWritingParameters(..)
  , withContents
  , blankListWritingParameters
  , WithListWriting
  , writeListOfThings
  , listWriterResponsesImpl
  , ListWriterResponses(..)
  ) where

import Yaifl.Prelude hiding (asks, Reader, runReader)

import Effectful.Reader.Static
import Effectful.Writer.Static.Local
import Yaifl.Game.Activities.PrintingInventoryDetails
import Yaifl.Game.Activities.PrintingRoomDescriptionDetails (WithPrintingRoomDescriptionDetails)
import Yaifl.Model.Actions.Args
import Yaifl.Model.Activity
import Yaifl.Core.HasProperty
import Yaifl.Model.Kinds
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Model.Kinds.Container
import Yaifl.Model.Kinds.Supporter
import Yaifl.Core.Metadata
import Yaifl.Model.Query
import Yaifl.Model.Rules.RuleEffects
import Yaifl.Model.WorldModel

import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.Responses
import Yaifl.Text.Say

import qualified Data.EnumSet as ES
import qualified Data.Text as T

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

newtype ListWriting wm = LW { responses :: ListWriterResponses -> Response wm (Thing wm) } deriving stock (Generic)
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
  writeAfterEntry cnt item

saySpace :: Writer Text :> es => Eff es ()
saySpace = tell " "

writeAfterEntry ::
  forall wm es.
  State (ListWritingVariables wm) :> es
  => Reader (ListWriting wm) :> es
  => Writer Text :> es
  => WithListWriting wm
  => RuleEffects wm es
  => Int
  -> Thing wm
  -> Eff es ()
writeAfterEntry _numberOfItem itemMember = do
  s <- lwp <$> get
  asThing <- getThingMaybe itemMember
  p <- getPlayer
  locP <- getLocation p
  -- if we are somehow writing about a room, ignore it.
  -- TODO: should we just cut out non-things here
  whenJust asThing $ \thingWrittenAbout -> do
    let lit = thingIsLit thingWrittenAbout
        asCont = getContainerMaybe thingWrittenAbout
        asSupporter = getSupporterMaybe thingWrittenAbout
    if
      | givingBriefInventoryInformation s -> do
        -- start the room description details activity
        -- note that the "false" here in the documentation
        -- means that the for rulebook came to no decision
        -- NOT that it doesn't exist or something
        -- https://ganelson.github.io/inform/WorldModelKit/S-lst.html
        -- so this is a mix of an override and also a falsely occupied/unoccupied check
          beginActivity #printingRoomDescriptionDetails thingWrittenAbout
          void $ whenHandling' #printingRoomDescriptionDetails $ do
            printBriefDetailsAbout locP thingWrittenAbout lit asCont
          void $ endActivity #printingRoomDescriptionDetails
      -- full inventory info (perhaps with nested children)
      | givingInventoryInformation s -> do
          beginActivity #printingInventoryDetails thingWrittenAbout
          void $ whenHandling' #printingInventoryDetails $ do
            printFullDetailsAbout thingWrittenAbout lit asCont
      | otherwise -> pass
    -- now we want to recurse properly
    whenJust (getEnclosingMaybe . toAny $ thingWrittenAbout) $ \enc -> do
      ListWritingParameters{..} <- use @(ListWritingVariables wm) #lwp
      containedThings <- mapM getThing $ ES.toList (view #contents enc)
      let nonConcealedThings = if notListingConcealedItems then
        -- there's also something about checking for containers?
        -- https://ganelson.github.io/inform/WorldModelKit/S-lst.html
        -- see: §10. Concealment.
        -- but at this point I'm just tired
            filter (\i -> thingIsConcealed i || thingIsScenery i) containedThings
            else containedThings
          numberOfThings = length nonConcealedThings
      recurseFlag1 <- if numberOfThings > 0 && includingAllContents
        then do
          when asEnglishSentence $ saySpace >> sayTellResponse Q thingWrittenAbout >> saySpace
          pure True
        else do
          pure False
      recurseFlag <- (|| recurseFlag1) <$> if numberOfThings > 0 && includingContents
        then
          -- the two branches are for a supporter and for an open transparent container
          if
            | isJust asSupporter -> do
                when asEnglishSentence $
                  if tersely
                    then sayTellResponse A thingWrittenAbout >> sayTellResponse R thingWrittenAbout
                    else sayTellResponse S thingWrittenAbout
                pure True
            | (\c -> isOpenContainer c || isTransparentContainer c) <$?> asCont -> do
                when asEnglishSentence $
                  if tersely
                    then sayTellResponse A thingWrittenAbout >> sayTellResponse T thingWrittenAbout
                    else sayTellResponse U thingWrittenAbout
                pure True
            | otherwise -> pure False
        else
          pure False
      when (recurseFlag && asEnglishSentence) $ whenJust (viaNonEmpty head nonConcealedThings) $ \thing1 -> do
        -- apparently we need to do this, because we want to say (on which ARE plurals) based on the first element of the list
        -- not on the plurality of the thing(s) doing the containing.
        regarding (Just thing1)
        sayTellResponse V thingWrittenAbout
        saySpace
      when withNewlines [sayingTell|#{linebreak}|]
      -- I have no idea what the original code did here but I think it's
      -- some iterator nonsense
      when recurseFlag $ do
        lwv <- get @(ListWritingVariables wm)
        let lwp' = lwp lwv
        -- TODO: the grouping should be here too
        writeListOfThings (lwp' { contents = map (SingleObject) nonConcealedThings, initialDepth = 1 + depth lwv})
        when tersely $ sayTellResponse B thingWrittenAbout
      pass

printBriefDetailsAbout ::
  Writer Text :> es
  => WithListWriting wm
  => Reader (ListWriting wm) :> es
  => RuleEffects wm es
  => Room wm
  -> Thing wm
  -> Bool
  -> Maybe Container
  -> Eff es ()
printBriefDetailsAbout locP thingWrittenAbout lit asCont = do
  let locationLit = roomIsLighted locP
      isCC = thingIsClosedContainer thingWrittenAbout
      visiblyEmpty = isEmptyContainer <$?> asCont
      transCont = isTransparentContainer <$?> asCont
      -- we have a total of 7 different possible combos
      -- giving light, closed container, visibly empty container that is not opaque and closed
      combo = (lit && not locationLit, isCC, (not isCC || transCont) && visiblyEmpty)
      anythingAtAll = view _1 combo || view _2 combo || view _3 combo
  when anythingAtAll $ sayTellResponse A thingWrittenAbout
  case combo of
    (True, False, False) -> sayTellResponse D thingWrittenAbout
    (False, True, False) -> sayTellResponse E thingWrittenAbout
    (True, True, False) -> sayTellResponse H thingWrittenAbout
    (False, False, True) -> sayTellResponse F thingWrittenAbout
    (True, False, True) -> sayTellResponse I thingWrittenAbout
    (False, True, True) -> sayTellResponse G thingWrittenAbout
    (True, True, True) -> sayTellResponse J thingWrittenAbout
    (False, False, False) -> pass
  when anythingAtAll $ sayTellResponse B thingWrittenAbout

printFullDetailsAbout ::
  Writer Text :> es
  => WithListWriting wm
  => Reader (ListWriting wm) :> es
  => RuleEffects wm es
  => Thing wm
  -> Bool
  -> Maybe Container
  -> Eff es ()
printFullDetailsAbout thingWrittenAbout lit asCont = do
  let isWorn = thingIsWorn thingWrittenAbout
      combo = lit || isWorn
  when combo $ sayTellResponse A thingWrittenAbout
  alreadyPrinted <- case (lit, isWorn) of
    (True, True) -> sayTellResponse K thingWrittenAbout >> pure True
    (True, False) -> sayTellResponse D thingWrittenAbout >> pure True
    (False, True) -> sayTellResponse L thingWrittenAbout >> pure True
    _ -> pure False
  printedContainerPart <- case asCont of
    Just container -> do
      if isOpenableContainer container
      then do
        -- either close off the clause or
        -- open a parenthesis
        if alreadyPrinted
        then do
          serialComma <- use @Metadata #oxfordCommaEnabled
          when serialComma $ tell ","
          sayTellResponse C thingWrittenAbout
        else sayTellResponse A thingWrittenAbout
        -- open check
        if isOpenContainer container
        then
          if isEmptyContainer container
          then sayTellResponse N thingWrittenAbout
          else sayTellResponse M thingWrittenAbout
        else
          if isLockedContainer container
          then sayTellResponse P thingWrittenAbout
          else sayTellResponse O thingWrittenAbout
        pure True
      else do
        when (isEmptyTransparentContainer container) $
          if alreadyPrinted
          then sayTellResponse C thingWrittenAbout >> sayTellResponse F thingWrittenAbout
          else sayTellResponse A thingWrittenAbout >> sayTellResponse F thingWrittenAbout >> sayTellResponse B thingWrittenAbout
        pure alreadyPrinted
    Nothing -> pure alreadyPrinted
  when printedContainerPart $ sayTellResponse B thingWrittenAbout

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

data ListWriterResponses = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | Y
  deriving stock (Show, Eq, Ord, Enum, Generic, Bounded, Read)

listWriterResponsesImpl :: ListWriterResponses -> Response wm (Thing wm)
listWriterResponsesImpl = \case
  A -> constResponse " ("
  B -> constResponse ")"
  C -> constResponse " and "
  D -> constResponse "providing light"
  E -> constResponse "closed"
  F -> constResponse "empty"
  G -> constResponse "closed and empty"
  H -> constResponse "closed and providing light"
  I -> constResponse "empty and providing light"
  J -> Response $ const $ do
    oc <- use @Metadata #oxfordCommaEnabled
    [sayingTell|closed, empty{?if oc},{?end if}|]
  R -> Response $ \o -> do
    p <- objectIsKind "person" o
    [sayingTell|on {?if p}whom{?else}which{?end if} |]
  T -> Response $ \o -> do
    p <- objectIsKind "person" o
    [sayingTell|in {?if p}whom{?else}which{?end if} |]
  -- "[regarding list writer internals][are] nothing" (W) TODO
  W -> constResponse "is nothing"
  -- "[regarding list writer internals][are]" (V) TODO
  V -> Response $ \_o -> do
    [sayingTell|#{are}|]
  Y -> constResponse "nothing"
  x -> constResponse $ "need to do response " <> show x <> " "

-- what else...probably want a test *now* for grouping items and for coalescing the list correctly