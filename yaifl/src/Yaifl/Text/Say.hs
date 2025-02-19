{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Yaifl.Text.Say
  ( SayArticle(..)
  , SayModal(..)
  , SayModalVerb(..)
  , SayLiteral(..)
  , SayableValue(..)
  , sayText
  , WithPrintingNameOfSomething
  , printingNameOfSomethingImpl
  , sayParameterName
  , module Yaifl.Text.SayQQ

  ) where

import Data.Char (toUpper)
import Data.Text.Display
import Effectful.Optics
import Effectful.Writer.Static.Local (Writer, tell, execWriter)
import GHC.TypeLits
import Yaifl.Prelude
import Yaifl.Core.Activity
import Yaifl.Core.Metadata
import Yaifl.Core.Kinds.Object
import Yaifl.Core.Effects
import Yaifl.Core.WorldModel
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Text.SayableValue
import Yaifl.Core.Rules.Rulebook
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.Print
import Yaifl.Text.Verb
import qualified Data.Text as T
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Core.Kinds.Room
import Yaifl.Core.Kinds.Thing
import Yaifl.Text.SayQQ
import Yaifl.Std.Kinds.Person ( getPersonMaybe, isMale, isFemale, Person )
import Yaifl.Core.ObjectLike
import Yaifl.Core.Actions.GoesWith

sayText ::
  SayableValue s wm
  => RuleEffects wm es
  => s
  -> Eff es Text
sayText = execWriter . sayTell

instance SayableValue a wm => SayableValue (Maybe a) wm where
  sayTell s = fromMaybe () <$> traverse sayTell s
  say s = whenJust s say

instance WithPrintingNameOfSomething wm => SayableValue (Room wm) wm where
  say = printName
  sayTell o = do
    t <- doActivity #printingNameOfSomething (toAny o)
    tell (fromMaybe "" t)

instance WithPrintingNameOfSomething wm => SayableValue (Thing wm) wm where
  say = printName
  sayTell o = do
    t <- doActivity #printingNameOfSomething (toAny o)
    tell (fromMaybe "" t)

instance WithPrintingNameOfSomething wm => SayableValue (AnyObject wm) wm where
  say = printName
  sayTell o = do
    t <- doActivity #printingNameOfSomething (toAny o)
    tell (fromMaybe "" t)

data SayingForm s =
  The s -- [The foo]
  | The_ s -- [the foo]
  | A s -- [A foo]
  | A_ s -- [a foo]


data SayArticle (article :: Symbol) v = SayArticle Bool v
data SayModal (modal :: Symbol) (verb :: Symbol) = SayModal Bool Bool
newtype SayModalVerb (modal :: Symbol) = SayModalVerb Bool
newtype SayLiteral (lit :: Symbol) = SayLiteral Bool

instance {-# OVERLAPS #-} ConjugatableVerb v => SayableValue (SayLiteral v) wm where
  sayTell (SayLiteral True) = sayTell @Text "Are"
  sayTell (SayLiteral False) = sayTell @Text "are"

instance (
  SayableValue (SayLiteral verb) wm
  , SayableValue (SayModalVerb modal) wm)
  => SayableValue (SayModal modal verb) wm where
  sayTell (SayModal modalCap verbCap) = do
    sayTell $ SayModalVerb @modal modalCap
    sayTell @Text " "
    sayTell $ SayLiteral @verb verbCap

withCapitalisation ::
  Writer Text :> es
  => Bool
  -> Text
  -> Eff es ()
withCapitalisation False t = tell t
withCapitalisation True t = tell $ t & _head  %~ toUpper

conjugateVerb ::
  forall wm es.
  State Metadata :> es
  => State (AdaptiveNarrative wm) :> es
  => VerbSense
  -> Verb
  -> Eff es Text
conjugateVerb sense (Verb _ conjugationTable) = do
    personage <- getPersonageOfObject
    t <- use @(AdaptiveNarrative wm) #tense
    pure $ runTabulation
      conjugationTable
        Active -- we never actually need Passive, I think.
        t
        sense
        personage

sayVerb ::
  forall v wm es.
  KnownSymbol v
  => RuleEffects wm es
  => Writer Text :> es
  => VerbSense
  -> SayLiteral v
  -> Eff es ()
sayVerb sense (SayLiteral cap) = do
    v <- conjugateVerb sense $ makeVerb (toText $ symbolVal (Proxy @v))
    withCapitalisation cap v

sayVerb' ::
  forall v wm es.
  KnownSymbol v
  => RuleEffects wm es
  => Writer Text :> es
  => SayLiteral v
  -> Eff es ()
sayVerb' = sayVerb Positive

instance SayableValue Int wm where
  sayTell x = tell (display x)

instance SayableValue (SayModalVerb "can't") wm where
  sayTell (SayModalVerb cap) = withCapitalisation cap "can't"

instance SayableValue (SayLiteral "we") wm where
  sayTell (SayLiteral cap) = do
    regardingThePlayer
    nv <- use @(AdaptiveNarrative wm) #narrativeViewpoint
    playerPronoun <- getPlayerPronoun
    withCapitalisation cap $ case nv of
      FirstPersonSingular -> "I"
      SecondPersonSingular -> "you"
      ThirdPersonSingular -> playerPronoun
      FirstPersonPlural -> "we"
      SecondPersonPlural -> "you"
      ThirdPersonPlural -> "they"

instance WMWithProperty wm Person => SayableValue (SayLiteral "they're") wm where
  sayTell (SayLiteral cap) = do
    mbO <- getMentioned

    case mbO of
      Nothing -> tell "it" -- I guess...
      Just o -> do
        p <- isPlayer o
        let persInfo = getPersonMaybe o
        if
        -- if the prior naming context is plural:
        -- say "they";
          | o ^. #namePlurality == PluralNamed -> withCapitalisation cap $ "they"
        -- otherwise if the item is the player:
          | p -> if cap then [saying|#{We}|] else [saying|#{we}|]
          | isMale <$?> persInfo -> withCapitalisation cap $ "he"
          | isFemale <$?> persInfo -> withCapitalisation cap $ "she"
          | otherwise -> withCapitalisation cap $ "that"
    [sayingTell|#{'re}|]

instance SayableValue (SayLiteral "linebreak") wm where
  sayTell _ = sayTell ("\n" :: Text)
  say _ = do
    void $ modifyBuffer (#lastMessageContext % #shouldPrintLinebreak .~ True)

instance SayableValue (SayLiteral "'re") wm where
  sayTell = sayVerb' @"'re"

instance SayableValue (SayLiteral "it") wm where
  sayTell (SayLiteral cap) = do
    regardingNothing
    withCapitalisation cap "it"

instance SayableValue (SayLiteral "see") wm where
  sayTell = sayVerb' @"see"

instance SayableValue (SayLiteral "go") wm where
  sayTell = sayVerb' @"go"

instance SayableValue (SayLiteral "can't") wm where
  sayTell = sayVerb @"can" Negative . coerce

instance SayableValue (SayLiteral "lead") wm where
  sayTell = sayVerb' @"lead"

instance SayableValue (SayLiteral "pass") wm where
  sayTell = sayVerb' @"pass"

instance SayableValue (SayLiteral "wait") wm where
  sayTell = sayVerb' @"wait"

instance SayableValue (SayLiteral "are") wm where
  sayTell s = sayVerb' @"be" (coerce s)

instance SayableValue (SayLiteral "can") wm where
  sayTell s = sayVerb' @"be able to" (coerce s)

instance SayableValue (SayLiteral "get") wm where
  sayTell s = sayVerb' @"get" (coerce s)

instance SayableValue (SayLiteral "look") wm where
  sayTell = sayVerb' @"look"

instance SayableValue (SayLiteral "open") wm where
  sayTell = sayVerb' @"open"

instance SayableValue (SayLiteral "close") wm where
  sayTell = sayVerb' @"close"

instance SayableValue (SayLiteral "switch") wm where
  sayTell = sayVerb' @"switch"

instance SayableValue (SayLiteral "paragraphBreak") wm where
  sayTell _ = sayTell ("\n\n" :: Text)
  say _ = do
    void $ modifyBuffer (#lastMessageContext % #shouldPrintPbreak .~ True)

instance SayableValue (SayLiteral "aren't") wm where
  sayTell = sayVerb @"aren't" Negative

getPlayerPronoun :: Eff es Text
getPlayerPronoun = pure "they"

instance {-# OVERLAPPABLE #-}
  ( ObjectLike wm a
  , WithPrintingNameOfSomething wm
  ) => SayableValue (SayArticle "the" a) wm where
  say (SayArticle c a) = if c then say (The a) else say (The_ a)
  sayTell (SayArticle c a) = if c then sayTell (The a) else sayTell (The_ a)

instance {-# OVERLAPPABLE #-}
  ( ObjectLike wm a
  , WithPrintingNameOfSomething wm
  ) => SayableValue (SayArticle "a" a) wm where
  say (SayArticle c a) = if c then say (A a) else say (A_ a)
  sayTell (SayArticle c a) = if c then sayTell (A a) else sayTell (A_ a)

instance (ObjectLike wm o, WithPrintingNameOfSomething wm) => SayableValue (SayingForm o) wm where
  sayTell s = do
    let (objLike, isDef, isCap) = getDetails s
    (o :: AnyObject wm) <- getObject objLike
    oName <- sayText $ o ^. #name
    let articleEff
          | o ^. #nameProperness == Proper = pure ""
          | isDef = pure "the"
          | o ^. #namePlurality == PluralNamed = pure "some"
          | Just x <- o ^. #indefiniteArticle = sayText x
          | (oName ^? _head) `elem` map Just ['a', 'i', 'e', 'o', 'u'] = pure "an"
          | otherwise = pure "a"
    article <- articleEff
    sayTell (if isCap then maybe "" (uncurry T.cons) (bimapF toUpper id (T.uncons article)) else article)
    when (article /= "") (sayTell @Text " ")
    ifM (isPlayer o)
      [sayingTell|#{We}|]
      (sayTell @(AnyObject wm) o)
    where
      getDetails = \case
        The a -> (a, True, True)
        The_ a -> (a, True, False)
        A a -> (a, False, True)
        A_ a -> (a, False, False)

type WithPrintingNameOfSomething wm = (Display (WMText wm), SayableValue (WMText wm) wm, WithActivity "printingNameOfSomething" wm () (AnyObject wm) Text)

-- TODO: https://ganelson.github.io/inform/BasicInformKit/S-prn.html#SP2
printName ::
  NoMissingObjects wm es
  => ActionHandler wm :> es
  => ObjectTraverse wm :> es
  => Print :> es
  => State (ActivityCollector wm) :> es
  => State (AdaptiveNarrative wm) :> es
  => Input :> es
  => State (ResponseCollector wm) :> es
  => WithPrintingNameOfSomething wm
  => ObjectLike wm o
  => o
  -> Eff es ()
printName o = do
  e <- getObject o
  t <- doActivity #printingNameOfSomething e
  let toSay = fromMaybe "" t
  [saying|{toSay}|]

printingNameOfSomethingImpl :: SayableValue (WMText s) s => Activity s () (AnyObject s) Text
printingNameOfSomethingImpl = (makeActivity "Printing the name of something"
    [makeRule "" [] (\o -> do
      regarding (Just o)
      t <- sayText $ o ^. #name
      pure $ Just t) ])
    { combineResults = \mbA mbB -> (<> (fromMaybe "" mbB))  <$> mbA }

sayParameterName ::
  NoMissingObjects wm es
  => ActionHandler wm :> es
  => ObjectTraverse wm :> es
  => Print :> es
  => State (ActivityCollector wm) :> es
  => State (AdaptiveNarrative wm) :> es
  => Input :> es
  => State (ResponseCollector wm) :> es
  => WithPrintingNameOfSomething wm
  => NamedActionParameter wm
  -> Eff es Text
sayParameterName (ObjectParameter o) = sayText (o ^. #name)
sayParameterName (ThingParameter o) = sayText (o ^. #name)
sayParameterName _ = pure "not done this one yet"