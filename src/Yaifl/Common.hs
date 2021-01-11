{- |
Copyright: (c) 2020 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <thecommunistduck@hotmail.co.uk>

Yet another interactive fiction library.
-}

module Yaifl.Common
    ( Entity
    , Store
    , emptyStore
    , RuleOutcome
    , RuleEvaluation
    , Rulebook(..)
    , Rule(..)
    , PlainRule
    , PlainRulebook
    {-
    , World(..)
    , Action(..)
    , worldToMapStore
    , newEntity
    , HasStore
    , store
    , EntityProducer
    , entityCounter
    , getEntityCounter
    , setEntityCounter
    , withEntityIDBlock
    , getStore
    , getComponent
    , getComponent'
    , setComponent
    , addComponent
    , adjustComponent
    , isX
    , GameSettings(..)
    , HasGameSettings
    , HasGameSettings'
    , title
    , rulebooks
    , globalComponent
    , firstRoom
    , SemWorld
    , SemWorldList
    -}
    ) where

import qualified Data.IntMap.Strict            as IM
import qualified Data.Map.Strict               as Map
import           Yaifl.Prelude
import           Yaifl.Say
import Control.Effect.Lens
import Control.Lens hiding (use)
import Control.Effect.State

{- TYPES -}

-- | An Entity is just an ID that is loosely associated with components. 
type Entity = Int

-- | Store a is a container of components of type a, indexed by entity ID.
-- in an ideal world (TODO? it'd have multiple different types of stores)
type Store a = IM.IntMap a

-- | A store with nothing in it
emptyStore :: Store a
emptyStore = IM.empty

globalComponent :: Int
globalComponent = 0

class HasStore w c where
    store :: Proxy c -> Lens' w (Store c)

class EntityProducer w where
    entityCounter :: Lens' w Entity

type HasWorld w sig m = Has (State w) sig m
-- | rules either give no outcome, true, or false.
type RuleOutcome = Bool
type RuleEvaluation m = m (Maybe RuleOutcome)

data Rule m v r where
    Rule :: Text -> m (Maybe r) -> Rule m () r
    RuleWithVariables :: Text -> State v m (Maybe r) -> Rule m v r

-- | a rulebook runs in a monadic context m with rulebook variables v and returns a value r, which is normally a success/fail
data Rulebook m v r where
    Rulebook :: Text -> Maybe r -> [Rule m () r] -> Rulebook m () r
    RulebookWithVariables :: Text -> Maybe r -> m (Maybe v) -> [Rule m v r] -> Rulebook m v r

type PlainRule m = Rule m () RuleOutcome
type PlainRulebook m = Rulebook m () RuleOutcome

getStore :: forall w sig m c. (Has (State w) sig m, HasStore w c) => Proxy c -> m (Store c)
getStore p = (use @w) $ store p

{-
rules :: Lens' (Rulebook w v r) [Rule w v r]
rules = lensVL $ \f s -> case s of
  Rulebook n d r -> fmap (Rulebook n d) (f r)
  RulebookWithVariables n d i r -> fmap (RulebookWithVariables n d i) (f r)
-}
{-
data World w m k where
    GetStore :: HasStore w c => Proxy c -> World w m (Store c)
    SetComponent :: HasStore w c => Proxy c -> Entity -> c -> World w m ()
    GetEntityCounter :: World w m Entity
    SetEntityCounter :: Entity -> World w m Entity

makeSem ''World

worldToMapStore :: EntityProducer w => Sem (World w ': r) a -> Sem (State w ': r) a
worldToMapStore = reinterpret $ \case
    GetStore p -> use $ store p
    SetComponent p e v -> store p % at' e ?= v
    GetEntityCounter   -> use entityCounter
    SetEntityCounter e -> do
        ec <- use entityCounter
        entityCounter .= e
        return ec

getComponent :: (HasStore w c, Members (SemWorldList w) r) => Proxy c -> Entity -> Sem r (Maybe c)
getComponent p e = do
    s <- getStore p
    return $ IM.lookup e s

getComponent' :: (HasStore w c, Members (SemWorldList w) r) => Proxy c -> Entity -> Sem r c
getComponent' p e = do
    s <- getStore p
    let res = maybeToRight "Somehow couldn't find the component" (IM.lookup e s)
    when (isLeft res) (
        logMsg Error $ "Attempted to find a component of " <>  show e <> " but couldn't find it")
    fromEither res

adjustComponent :: (HasStore w c, Members (SemWorldList w) r) => Proxy c -> Entity -> (c -> c) -> Sem r ()
adjustComponent p e f = do
    c <- getComponent p e
    whenJust c (setComponent p e . f)

type SemWorldList w = '[World w, Log, State LoggingContext, Say, State (GameSettings w), Error Text]

type SemWorld w r = Sem (SemWorldList w) r

data GameSettings w = GameSettings
    { _title     :: Text
    , _firstRoom :: Maybe Entity
    , _rulebooks :: Map.Map Text (RuleEvaluation w)
    }
type HasGameSettings w r = Members (SemWorldList w) r
type HasGameSettings' w = HasGameSettings w (SemWorldList w)



data Activity w v = Activity
    { _activityName :: Text
    , _initActivity :: [Entity] -> SemWorld w (Maybe v)
    , _beforeRules  :: Rulebook w v (v, RuleOutcome)
    , _forRules     :: Rulebook w v (v, RuleOutcome)
    , _afterRules   :: Rulebook w v (v, RuleOutcome)
    }

data Action w v = Action
    { _actionName          :: Text
    , _understandAs        :: [Text]
    , _setActionVariables  :: [Entity] -> Rulebook w v (Maybe v)
    , _beforeActionRules   :: Rulebook w v (v, RuleOutcome)
    , _checkActionRules    :: Rulebook w v (v, RuleOutcome)
    , _carryOutActionRules :: Rulebook w v (v, RuleOutcome)
    , _reportActionRules   :: Rulebook w v RuleOutcome
    }

class Eq a => ActionArgs a where
    unboxArguments :: [Entity] -> Maybe a

-- | just default arguments
class RulebookArgs a where
    defaultArguments :: a

instance ActionArgs Entity where
    unboxArguments [a] = Just a
    unboxArguments _   = Nothing

instance ActionArgs () where
    unboxArguments [] = Just ()
    unboxArguments _  = Nothing

withEntityIDBlock :: Member (World w) r => Entity -> Sem r a -> Sem r a
withEntityIDBlock idblock x = do
    ec <- setEntityCounter idblock
    v  <- x
    _  <- setEntityCounter ec
    return v

newEntity :: Member (World w) r => Sem r Entity
newEntity = do
    ec <- getEntityCounter
    _  <- setEntityCounter (ec + 1)
    return ec

-- TODO: check it doesn't have the component to begin with
addComponent :: (Member (World w) r, HasStore w c) => Entity -> c -> Sem r ()
addComponent = setComponent (Proxy :: Proxy c)

isX :: (Eq b, Members (SemWorldList w) r, HasStore w c) => b -> (c -> b) -> Proxy c -> Entity -> Sem r Bool
isX p recordField comp e =
    (Just p ==) . fmap recordField <$> getComponent comp e

makeLenses ''Rulebook
makeLenses ''GameSettings
-}
