{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Yaifl.Lamp.Say where

import Data.Text.Display
import Solitude
import Yaifl.Core.Actions.Activity
import Yaifl.Core.AdaptiveNarrative
import Yaifl.Core.Object
import Yaifl.Core.Objects.Query
import Yaifl.Core.Print
import Yaifl.Core.Rules.Rule
import Yaifl.Core.WorldModel
import Data.Char (toUpper)
import qualified Data.Text as T
import Yaifl.Core.Rules.RuleEffects
import GHC.TypeLits
import Effectful.Optics (use)
import Effectful.Writer.Static.Local (Writer, tell)

instance SayableValue a wm => SayableValue (Maybe a) wm where
  sayTell s = fromMaybe () <$> traverse sayTell s
  say s = whenJust s say

instance WithPrintingNameOfSomething wm => SayableValue (Room wm) wm where
  say = printName
  sayTell o = sayTell $ o ^. #name

instance WithPrintingNameOfSomething wm => SayableValue (Thing wm) wm where
  say = printName
  sayTell o = sayTell $ o ^. #name

instance WithPrintingNameOfSomething wm => SayableValue (AnyObject wm) wm where
  say = printName
  sayTell o = sayTell $ o ^. #name

data SayingForm s =
  The s -- [The foo]
  | The_ s -- [the foo]
  | A s -- [A foo]
  | A_ s -- [a foo]

class ConjugatableVerb (v :: Symbol) where
  toVerb :: Proxy v -> Verb

data Verb = Verb
  {

  }

data SayArticle (article :: Symbol) wm d = SayArticle Bool (Object wm d)
data SayModal (modal :: Symbol) (verb :: Symbol) = SayModal Bool Bool
newtype SayModalVerb (modal :: Symbol) = SayModalVerb Bool
newtype SayLiteral (lit :: Symbol) = SayLiteral Bool

instance {-# OVERLAPPABLE #-} (TypeError
  ('Text "Cannot conjugate the verb 'to " ':<>: 'ShowType a ':<>:
    'Text "'; perhaps you are missing a SayableValue (SayLiteral " ':<>:
    'ShowType a ':<>: 'Text ") instance if this is not a verb?")) => ConjugatableVerb a where
  toVerb :: Proxy a -> Verb
  toVerb = error "impossible"

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
  Bool
  -> Text
  -> Eff (Writer Text : es) ()
withCapitalisation False t = tell t
withCapitalisation True t = tell $ t & _head  %~ toUpper

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

getPlayerPronoun :: Eff es Text
getPlayerPronoun = pure "they"

instance
  ( ObjectLike wm (Object wm o)
  , WithPrintingNameOfSomething wm
  ) => SayableValue (SayArticle "the" wm o) wm where
  say (SayArticle c a) = if c then say (The a) else say (The_ a)
  sayTell (SayArticle c a) = if c then sayTell (The a) else sayTell (The_ a)


instance (ObjectLike wm o, WithPrintingNameOfSomething wm) => SayableValue (SayingForm o) wm where
  sayTell s = do
    let (objLike, isDef, isCap) = getDetails s
    (o :: AnyObject wm) <- getObject objLike
    let articleEff
          | isDef = (if o ^. #nameProperness == Proper
              then pure ""
              else pure "the"
            )
          | o ^. #namePlurality == PluralNamed = pure "some"
          | Just x <- o ^. #indefiniteArticle = sayText x
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

type WithPrintingNameOfSomething wm = (Display (WMSayable wm), SayableValue (WMSayable wm) wm, WithActivity "printingNameOfSomething" wm (AnyObject wm) ())

printName ::
  NoMissingObjects wm es
  => Print :> es
  => ActionHandler wm :> es
  => ObjectTraverse wm :> es
  => State (ActivityCollector wm) :> es
  => State (ResponseCollector wm) :> es
  => State (AdaptiveNarrative wm) :> es
  => WithPrintingNameOfSomething wm
  => ObjectLike wm o
  => o
  -> Eff es ()
printName o = do
  e <- getObject o
  void $ doActivity #printingNameOfSomething e

printingNameOfSomethingImpl :: Activity s (AnyObject s) ()
printingNameOfSomethingImpl = makeActivity "Printing the name of something"
    [makeRule "" (\o -> do
      t <- sayText $ o ^. #name
      printText t >> return (Just ())) ]