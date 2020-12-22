{- |
Copyright: (c) 2020 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <thecommunistduck@hotmail.co.uk>

Yet another interactive fiction library.
-}

module Yaifl.Common2
    ( Entity
    , Store
    , emptyStore
    , RuleOutcome
    , RuleEvaluation
    , Rulebook(..)
    , rules
    , Rule(..)
    , RuleFunction(..)
    , PlainRule
    , PlainRulebook
    , World(..)
    , worldToMapStore
    , newEntity
    , HasStore
    , store
    , EntityProducer
    , entityCounter
    , getEntityCounter
    , setEntityCounter
    , withEntityIDBlock
    , getComponent
    , getComponent'
    , setComponent
    , addComponent
    , isX
    , GameSettings(..)
    , HasGameSettings
    , HasGameSettings'
    , title
    , rulebooks
    , firstRoom
    , SemWorld
    , SemWorldList
    ) where

import qualified Data.IntMap.Strict            as IM
import qualified Data.Map.Strict               as Map
import           Polysemy.State
import           Polysemy.Error
import           Yaifl.Prelude
import           Yaifl.Say2
import Yaifl.PolysemyOptics

{- TYPES -}

-- | An Entity is just an ID that is loosely associated with components. 
type Entity = Int

-- | Store a is a container of components of type a, indexed by entity ID.
-- in an ideal world (TODO? it'd have multiple different types of stores)
type Store a = IM.IntMap a

-- | A store with nothing in it
emptyStore :: Store a
emptyStore = IM.empty

class HasStore w c where
    store :: Proxy c -> Lens' w (Store c)

class EntityProducer w where
    entityCounter :: Lens' w Entity

data World w m k where
    SetComponent :: HasStore w c => Proxy c -> Entity -> c -> World w m ()
    GetComponent :: HasStore w c => Proxy c -> Entity -> World w m (Maybe c)
    GetComponent' :: HasStore w c => Proxy c -> Entity -> World w m c
    GetEntityCounter :: World w m Entity
    SetEntityCounter :: Entity -> World w m Entity

makeSem ''World

worldToMapStore :: (Member (Error Text) r, EntityProducer w) => Sem (World w ': r) a -> Sem (State w ': r) a
worldToMapStore = reinterpret $ \case
    SetComponent p e v -> store p % at' e ?= v
    GetComponent p e   -> use (store p % at' e)
    GetEntityCounter   -> use entityCounter
    SetEntityCounter e -> do
        ec <- use entityCounter
        entityCounter .= e
        return ec
    GetComponent' p e -> do
        v <- use $ store p % at' e
        fromEither $ maybeToRight "Somehow couldn't find the component" v

type SemWorldList w = '[World w, Log, State LoggingContext, Say, State (GameSettings w), Error Text]

type SemWorld w r = Sem (SemWorldList w) r

data GameSettings w = GameSettings
    { _title     :: Text
    , _firstRoom :: Maybe Entity
    , _rulebooks :: Map.Map Text (RuleEvaluation w)
    }
type HasGameSettings w r = Members (SemWorldList w) r
type HasGameSettings' w = HasGameSettings w (SemWorldList w)

-- | rules either give no outcome, true, or false.
type RuleOutcome = Bool
type RuleEvaluation w = SemWorld w (Maybe RuleOutcome)

data Rule m v r = Rule Text (RuleFunction m v r)

data RuleFunction w v r = RuleWithVariables (Sem (State v ': SemWorldList w) (Maybe r)) | NormalRule (SemWorld w (Maybe r))

-- | a rulebook runs in a monadic context m with rulebook variables v and returns a value r, which is normally a success/fail
data Rulebook w v r = Rulebook
    {
    -- | printed name of the rulebook
      _rulebookName :: Text
    , _defaultOutcome :: Maybe r
    -- | a way to construct the initial rulebook variables
    ,  _rulebookInit :: SemWorld w v
    -- | the list of rules and their name
    , _rules :: [Rule w v r]
    }

type PlainRule w = Rule w () RuleOutcome
type PlainRulebook w = Rulebook w () RuleOutcome

data Activity w v = Activity
    { _activityName :: Text
    , _initActivity :: [Entity] -> SemWorld w (Maybe v)
    , _beforeRules  :: Rulebook w v (v, RuleOutcome)
    , _forRules     :: Rulebook w v (v, RuleOutcome)
    , _afterRules   :: Rulebook w v (v, RuleOutcome)
    }

data Action w v r = Action
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

isX
    :: (Eq b, Member (World w) r, HasStore w c)
    => b
    -> (c -> b)
    -> Proxy c
    -> Entity
    -> Sem r Bool
isX p recordField comp e =
    (Just p ==) . fmap recordField <$> getComponent comp e

makeLenses ''Rulebook
makeLenses ''GameSettings
