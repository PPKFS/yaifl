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

  ) where

import Data.Char (toUpper)
import Data.Text.Display
import Effectful.Optics
import Effectful.Writer.Static.Local (Writer, tell, execWriter)
import GHC.TypeLits
import Solitude
import Yaifl.Model.Activity
import Yaifl.Model.Metadata
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Effects
import Yaifl.Model.Query
import Yaifl.Model.WorldModel
import Yaifl.Model.Rules.Rulebook
import Yaifl.Model.Rules.RuleEffects
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.Print
import Yaifl.Text.Verb
import qualified Data.Text as T
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Kinds.Room
import Yaifl.Model.Kinds.Thing
import Yaifl.Text.SayQQ

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
  sayTell = tellName

instance WithPrintingNameOfSomething wm => SayableValue (Thing wm) wm where
  say = printName
  sayTell = tellName

instance WithPrintingNameOfSomething wm => SayableValue (AnyObject wm) wm where
  say = printName
  sayTell = tellName

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
  => SayLiteral v
  -> Eff es ()
sayVerb (SayLiteral cap) = do
    v <- conjugateVerb Positive $ makeVerb (toText $ symbolVal (Proxy @v))
    withCapitalisation cap v

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

instance SayableValue (SayLiteral "linebreak") wm where
  sayTell _ = sayTell ("\n" :: Text)
  say _ = do
    void $ modifyBuffer (#lastMessageContext % #shouldPrintLinebreak .~ True)

instance SayableValue (SayLiteral "it") wm where
  sayTell (SayLiteral cap) = do
    regardingNothing
    withCapitalisation cap "it"

instance SayableValue (SayLiteral "see") wm where
  sayTell = sayVerb @"see"

instance SayableValue (SayLiteral "go") wm where
  sayTell = sayVerb @"go"

instance SayableValue (SayLiteral "can't") wm where
  sayTell = sayVerb @"can't"

instance SayableValue (SayLiteral "lead") wm where
  sayTell = sayVerb @"lead"

instance SayableValue (SayLiteral "are") wm where
  sayTell s = sayVerb @"be" (coerce s)

instance SayableValue (SayLiteral "can") wm where
  sayTell s = sayVerb @"be able to" (coerce s)

instance SayableValue (SayLiteral "look") wm where
  sayTell = sayVerb @"look"

instance SayableValue (SayLiteral "open") wm where
  sayTell = sayVerb @"open"

instance SayableValue (SayLiteral "close") wm where
  sayTell = sayVerb @"close"

instance SayableValue (SayLiteral "paragraphBreak") wm where
  sayTell _ = sayTell ("dddddd\n\n" :: Text)
  say _ = do
    void $ modifyBuffer (#lastMessageContext % #shouldPrintPbreak .~ True)

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
    sayTell @(AnyObject wm) o
    where
      getDetails = \case
        The a -> (a, True, True)
        The_ a -> (a, True, False)
        A a -> (a, False, True)
        A_ a -> (a, False, False)

type WithPrintingNameOfSomething wm = (Display (WMSayable wm), SayableValue (WMSayable wm) wm, WithActivity "printingNameOfSomething" wm () (AnyObject wm) Text)

-- TODO: https://ganelson.github.io/inform/BasicInformKit/S-prn.html#SP2
printName ::
  NoMissingObjects wm es
  => ActionHandler wm :> es
  => ObjectTraverse wm :> es
  => Print :> es
  => State (ActivityCollector wm) :> es
  => State (AdaptiveNarrative wm) :> es
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

tellName ::
  NoMissingObjects wm es
  => ActionHandler wm :> es
  => ObjectTraverse wm :> es
  => Writer Text :> es
  => Print :> es
  => State (ActivityCollector wm) :> es
  => State (AdaptiveNarrative wm) :> es
  => State (ResponseCollector wm) :> es
  => WithPrintingNameOfSomething wm
  => ObjectLike wm o
  => o
  -> Eff es ()
tellName o = do
  e <- getObject o
  t <- doActivity #printingNameOfSomething e
  let toSay = fromMaybe "" t
  [sayingTell|{toSay}|]

printingNameOfSomethingImpl :: SayableValue (WMSayable s) s => Activity s () (AnyObject s) Text
printingNameOfSomethingImpl = makeActivity "Printing the name of something"
    [makeRule "" [] (\o -> do
      regarding (Just o)
      t <- sayText $ o ^. #name
      pure $ Just t) ]