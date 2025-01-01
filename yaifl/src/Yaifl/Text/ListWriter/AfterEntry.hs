{-# LANGUAGE RecordWildCards #-}
module Yaifl.Text.ListWriter.AfterEntry
  ( writeAfterEntry

  ) where


import Yaifl.Prelude hiding (asks, Reader, runReader)

import Effectful.Reader.Static
import Effectful.Writer.Static.Local
import Yaifl.Core.Activity
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Std.Kinds.Container
import Yaifl.Std.Kinds.Supporter
import Yaifl.Core.Metadata
import Yaifl.Core.Query.Object

import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.Responses
import Yaifl.Text.Say

import qualified Data.EnumSet as ES
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Kinds.Room
import Yaifl.Core.Query.Enclosing
import Yaifl.Core.ObjectLike
import Yaifl.Std.Kinds.Person
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Text.ListWriter.Parameters
import Yaifl.Text.ListWriter.Responses

saySpace :: Writer Text :> es => Eff es ()
saySpace = tell " "


writeAfterEntry ::
  forall wm es.
  State (ListWritingVariables wm) :> es
  => Reader (ListWriting wm) :> es
  => Writer Text :> es
  => WithListWriting wm
  => RuleEffects wm es
  => (ListWritingParameters wm -> Eff es ())
  -> Int
  -> Thing wm
  -> Eff es ()
writeAfterEntry writeListOfThings _numberOfItem itemMember = do
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