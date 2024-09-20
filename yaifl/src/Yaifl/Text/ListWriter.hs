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

import Yaifl.Prelude hiding (Reader, runReader)
import Yaifl.Model.Rules.RuleEffects
--import Effectful.Writer.Static.Local ( tell )
import Yaifl.Text.Say
import Yaifl.Model.Activity
import Yaifl.Text.Responses
import Effectful.Writer.Static.Local
import Yaifl.Model.Metadata
import qualified Data.Text as T
import Effectful.Reader.Static
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.WorldModel
import Yaifl.Game.Activities.PrintingRoomDescriptionDetails (WithPrintingRoomDescriptionDetails)
import Yaifl.Model.Query
import Yaifl.Model.Kinds (Object(objectData))
import Yaifl.Model.Actions.Args
import Yaifl.Model.Kinds.Room
import Yaifl.Model.Kinds.Container
import Yaifl.Model.HasProperty
import Yaifl.Model.Kinds.Thing

type WithListWriting wm = (
  WithPrintingNameOfSomething wm
  , WithActivity "listingContents" wm () (ListWritingParameters wm) ()
  , WithActivity "groupingTogether" wm () (AnyObject wm) ()
  , WithActivity "printingANumberOf" wm () (Int, AnyObject wm) ()
  , WithResponseSet wm An_Iso "listWriterResponses" (ListWriterResponses -> Response wm ())
  , WithPrintingRoomDescriptionDetails wm
  , WMWithProperty wm Container
  )

newtype ListWriting wm = LW { responses :: ListWriterResponses -> Response wm () } deriving stock (Generic)
data ListWritingItem wm =
  SingleObject (AnyObject wm)
  -- a group may contain equivalence classes of objects
  | GroupedItems (NonEmpty (ListWritingItem wm))
  | EquivalenceClass (NonEmpty (AnyObject wm))

instance Display (WMText wm) => Display (ListWritingItem wm) where
  displayBuilder (SingleObject o) = displayBuilder o
  displayBuilder _ = "some list items"

data ListWritingParameters (wm :: WorldModel) = ListWritingParameters
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
    , depth = 0
    , margin = if withExtraIndentation lwp then 1 else 0
    , lwp = lwp
    }) $ do
    case contents lwp of
      [] -> do
        if prefacingWithIsAre lwp
        then sayTellResponse W ()
        else sayTellResponse Y ()
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
    sayTellResponse V ()
    if withNewlines
    then tell ":\n"
    else tell " "
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
        sayTellResponse C ()
      when (i < (length adjustedList - 1)) $ tell ", "

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
  let getObjectOut = \case
        SingleObject o -> o
        GroupedItems (g :| _) -> getObjectOut g
        EquivalenceClass (g :| _) -> g
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

writeAfterEntry ::
  forall wm es.
  State (ListWritingVariables wm) :> es
  => Reader (ListWriting wm) :> es
  => Writer Text :> es
  => WithListWriting wm
  => RuleEffects wm es
  => Int
  -> AnyObject wm
  -> Eff es ()
writeAfterEntry numberOfItem itemMember = do
  s <- lwp <$> get
  asThing <- getThingMaybe itemMember
  p <- getPlayer
  locP <- getLocation p
  -- if we are somehow writing about a room, ignore it.
  -- TODO: should we just cut out non-things here
  whenJust asThing $ \thingWrittenAbout -> do
    let lit = thingIsLit thingWrittenAbout
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
            let locationLit = roomIsLighted locP
                asCont = getContainerMaybe thingWrittenAbout
                isCC = thingIsClosedContainer thingWrittenAbout
                opClosed = isOpaqueClosedContainer <$> asCont
                visiblyEmpty = isEmptyContainer <$?> asCont
                -- we have a total of 7 different possible combos
                -- giving light, closed container, visibly empty container that is not opaque and closed
                combo = (lit && not locationLit, isCC, Just False == opClosed && visiblyEmpty)
                anythingAtAll = view _1 combo || view _2 combo || view _3 combo
            when anythingAtAll $ sayTellResponse A ()
            case combo of
              (True, False, False) -> sayTellResponse D ()
              (False, True, False) -> sayTellResponse E ()
              (True, True, False) -> sayTellResponse H ()
              (False, False, True) -> sayTellResponse F ()
              (True, False, True) -> sayTellResponse I ()
              (False, True, True) -> sayTellResponse G ()
              (True, True, True) -> sayTellResponse J ()
              (False, False, False) -> pass
            when anythingAtAll $ sayTellResponse B ()
          void $ endActivity #printingRoomDescriptionDetails
      -- full inventory info (perhaps with nested children)
      | givingInventoryInformation s -> do
          beginActivity #printingInventoryDetails thingWrittenAbout
          void $ whenHandling' #printingInventoryDetails $ do
            let isWorn = thingIsWorn thingWrittenAbout
                combo = lit || isWorn
            when combo $ sayTellResponse A ()
            case (lit, isWorn) of
              (True, True) ->
            pass
      | otherwise -> pass
-- combos are as follows
{-
then for full inventory
light and worn: A then K (providing light and being worn))
light: D
worn: A, being worn L
container:
  if openable: (I guess to aovid supporters)
    potential and (C) or open the bracket (A)
    if open and has stuff:
      open (M)
      open but empty (N)
    if locked:
      closed and locked (P)
    else:
      closed (O)
  otherwise it's a supporter:
    transparent and without children,
       "and" if needbe (C) and empty (F)
        ( (A), empty (F), ) (B)

if (parenth_flag) LW_Response('B');

loop over all children of o
ignore concealed objects
if there's stuff print containing (Q)
if it's a supporter then
  A-R-S
  (on which/on top of which )
otherwise I-T-U
  (in which/inside of which)
  regard the first item in the list
  then recurse and print a list of things
  followed by finally the closing bracket
-}

{-
rulebooks
turn sequence
- follow the every turn rules
- follow the scene changing rules
- generate
scene changing
when play begins
when play ends
when scene begins
when scene ends
every turn

some action processing
action awareness
accessibility rules
reaching inside
reaching outside
visibility rules
persuasion rules
unsuccessful attempt
does the player mean
multiple action processing

-}

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

data ListWriterResponses = A | B | C | D | E | F | G | H | I | J | W | V | Y

listWriterResponsesImpl :: ListWriterResponses -> Response wm ()
listWriterResponsesImpl = \case
  A -> constResponse " ("
  C -> constResponse " and "
  -- "[regarding list writer internals][are] nothing" (W) TODO
  W -> constResponse "is nothing"
  -- "[regarding list writer internals][are]" (V) TODO
  V -> constResponse "is"
  Y -> constResponse "nothing"

-- what else...probably want a test *now* for grouping items and for coalescing the list correctly