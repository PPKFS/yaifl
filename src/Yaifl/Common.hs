{- |
Copyright: (c) 2020 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <thecommunistduck@hotmail.co.uk>

Yet another interactive fiction library.
-}

module Yaifl.Common 
(
    --basic components
    Entity, Store, emptyStore, Name, Description(..), --_entityID, _entityType, entityID, entityType, 
    --rulebooks
    RuleOutcome, RuleEvaluation, System, UncompiledRulebook(..), Rulebook(..),
    Activity(..), UncompiledActivity(..),
    Action(..), UncompiledAction(..),
    
    --game info fields and lenses
    GameInfo, RoomDescriptions(..), blankGameInfo, gameInfo,
    _title, _firstRoom, _activities, _rulebooks, _actions, 
    _roomDescriptions, _darknessWitnessed, _localePriorities,
    title, firstRoom, activities, rulebooks, actions, 
    roomDescriptions, darknessWitnessed, localePriorities,
    setTitle,

    ActionArgs, unboxArguments, RulebookArgs, defaultArguments, defaultActionArguments,
    Has,HasWorld, HasWorld', HasGameInfo, HasGameInfo',

    world, store,

    HasID, objID,
    newEntity,

    tryAction,
    lookingActionName
) where

import Relude
import Yaifl.Say
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as Map
import qualified Text.Show
import Control.Lens

{- TYPES -}

-- | an entity is just an ID that is loosely associated with components.
{-
data Entity = Entity
    {
        -- | the ID of an entity 
        _entityID :: Int,
        -- | a semi-optional 'promise' that allows for lookups of multiple types or kinds
        -- where the 'promise' is a userspace invariant. Assuming nobody starts wrongly deleting
        -- things, then we can use partial functions.
        _entityType :: Atom.Symbol
    } deriving Show
-}
type Entity = Int

--instance Eq Entity where
--    (==) a b = _entityID a == _entityID b

-- | Store a is a container of components of type a, indexed by entity ID
-- in an ideal world (TODO? it'd have multiple different types of stores)
type Store a = IntMap a
-- | A store with nothing in it
emptyStore :: Store a
emptyStore = IM.empty

-- the printed name of something
type Name = Text

-- | rules either give no outcome, true, or false.
type RuleOutcome = Maybe Bool

-- | systems are just state monads over a world w (or maybe a world with extra stuff) that return a
type System w a = StateT w Identity a

-- | evaluating a rule is a state over a world with a rule outcome.
type RuleEvaluation w = System w RuleOutcome

-- | a rulebook over a world w with rulebook variables r.
data UncompiledRulebook w r = Rulebook
    {
        -- | printed name of the rulebook
        _rulebookName :: Text,
        -- | a way to construct the initial rulebook variables
        _rulebookInit :: w -> r,
        -- | the list of rules and their name
        _rules :: [(Text, RuleEvaluation (w, r))]
    }
-- | we can compile rulebooks down to avoid internal state
newtype Rulebook w = CompiledRulebook { getRule :: RuleEvaluation w }

-- | an activity running in world w with rulebook variables r and with parameters p
data UncompiledActivity w r p = Activity
    {
        _activityName :: Text,
        _initActivity :: w -> r,
        _beforeRules :: (p, r) -> UncompiledRulebook w (p, r),
        _forRules :: (p, r) -> UncompiledRulebook w (p, r),
        _afterRules :: (p, r) -> UncompiledRulebook w (p, r)
    }
newtype Activity w = CompiledActivity ([Entity] -> RuleEvaluation w)

-- | an action running in world w with rulebook variables r and with parameters p
-- very similar to an activity, except with more rulebooks and a whole rulebook
-- to set the variables 
data UncompiledAction w r p = Action
    {
        _actionName :: Text,
        _understandAs :: [Text],
        _setActionVariables :: p -> UncompiledRulebook w (p, r),
        _beforeActionRules :: (p, r) -> UncompiledRulebook w (p, r),
        _checkActionRules :: (p, r) -> UncompiledRulebook w (p, r),
        _carryOutActionRules :: (p, r) -> UncompiledRulebook w (p, r),
        _reportActionRules :: (p, r) -> UncompiledRulebook w (p, r)
    }
newtype Action w = CompiledAction ([Entity] -> RuleEvaluation w)

blankGameInfo :: GameInfo w
blankGameInfo = GameInfo "" Nothing blankMessageBuffer 1 Map.empty Map.empty Map.empty 
                    NeverAbbreviatedRoomDescriptions False Map.empty

instance Show (UncompiledRulebook w r) where
    show (Rulebook n _ r) = toString n <> concatMap (toString . fst) r
instance Show (UncompiledActivity w r p) where
    show = show . _activityName
instance Show (UncompiledAction w r p) where
    show = show . _actionName

instance Show (Rulebook w) where
    show _ = "compiled rulebook"
instance Show (Activity w) where
    show _ = "compiled activity"
instance Show (Action w) where
    show _ = "compiled action"

-- | big container of general game /stuff/
data GameInfo w = GameInfo 
    {
        _title :: Text,
        _firstRoom :: Maybe Entity,
        _msgBuffer :: MessageBuffer,
        _entityCounter :: Int,
        _activities :: Map.Map Text (Activity w),
        _rulebooks :: Map.Map Text (Rulebook w),
        _actions :: Map.Map Text (Action w),
        _roomDescriptions :: RoomDescriptions,
        _darknessWitnessed :: Bool,
        _localePriorities :: Map.Map Entity Int
    } deriving Show

data RoomDescriptions = AbbreviatedRoomDescriptions | SometimesAbbreviatedRoomDescriptions
                            | NeverAbbreviatedRoomDescriptions deriving (Eq, Show)

setTitle :: HasGameInfo a w  => Text -> System a ()
setTitle t = gameInfo . title .= t

-- | basically just manually writing a lens typeclass for...reasons I cannot remember
-- I really don't know why, but it's sort of necessary to manually write it out.
class HasMessageBuffer u => HasGameInfo u w | u -> w where
    gameInfo :: Lens' u (GameInfo w)
    title :: Lens' u Text
    title = gameInfo . go where go f g@GameInfo{..} = (\x' -> g {_title = x'}) <$> f _title
    firstRoom :: Lens' u (Maybe Entity)
    firstRoom = gameInfo . go where go f g@GameInfo{..} = (\x' -> g {_firstRoom = x'}) <$> f _firstRoom
    msgBuffer :: Lens' u MessageBuffer
    msgBuffer = gameInfo . go where go f g@GameInfo{..} = (\x' -> g {_msgBuffer = x'}) <$> f _msgBuffer
    entityCounter :: Lens' u Int
    entityCounter = gameInfo . go where go f g@GameInfo{..} = (\x' -> g {_entityCounter = x'}) <$> f _entityCounter
    rulebooks :: Lens' u (Map.Map Text (Rulebook w))
    rulebooks = gameInfo . go where go f g@GameInfo{..} = (\x' -> g {_rulebooks = x'}) <$> f _rulebooks
    activities :: Lens' u (Map.Map Text (Activity w))
    activities = gameInfo . go where go f g@GameInfo{..} = (\x' -> g {_activities = x'}) <$> f _activities
    actions :: Lens' u (Map.Map Text (Action w))
    actions = gameInfo . go where go f g@GameInfo{..} = (\x' -> g {_actions = x'}) <$> f _actions
    darknessWitnessed :: Lens' u Bool
    darknessWitnessed = gameInfo . go where go f g@GameInfo{..} = (\x' -> g {_darknessWitnessed = x'}) <$> f _darknessWitnessed
    roomDescriptions :: Lens' u RoomDescriptions
    roomDescriptions = gameInfo . go where go f g@GameInfo{..} = (\x' -> g {_roomDescriptions = x'}) <$> f _roomDescriptions
    localePriorities :: Lens' u (Map.Map Entity Int)
    localePriorities = gameInfo . go where go f g@GameInfo{..} = (\x' -> g {_localePriorities = x'}) <$> f _localePriorities
    
-- | some lens typeclasses for world access
-- the general form is a (u)niverse has a (w)orld for HasWorld 
-- but we can zoom in and specify that our universe is just the world and nothing else
class HasGameInfo u w => HasWorld u w | u -> w where
    world :: Lens' u w

instance HasWorld w w => HasWorld (w, b) w where
    world = _1 . world

type HasWorld' w = (HasWorld w w, HasGameInfo' w)
type HasGameInfo' w = HasGameInfo w w

instance HasGameInfo w w => HasGameInfo (w, a) w where
    gameInfo = _1 . gameInfo
instance HasGameInfo (GameInfo w) w where
    gameInfo = id
instance HasMessageBuffer (GameInfo w) where
    messageBuffer = msgBuffer

-- | component lookup typeclass
class Has w c where
    store :: Proxy c -> Lens' w (Store c)

-- | allows us to treat object components and entities identically
class HasID o where
    objID :: o -> Int
instance HasID Entity where
    objID = id --_entityID

-- | Action args are a way to move from a list of entities to, maybe, the correct
-- number of arguments for an action
class Eq a => ActionArgs a where
    unboxArguments :: [Entity] -> Maybe a
    defaultActionArguments :: a

-- | just default arguments
class RulebookArgs a where
    defaultArguments :: a

instance ActionArgs Entity where
    unboxArguments [a] = Just a
    unboxArguments _ = Nothing
    defaultActionArguments = -1

instance ActionArgs () where
    unboxArguments [] = Just ()
    unboxArguments _ = Nothing
    defaultActionArguments = ()

-- | descriptions are either plain, or they can be dynamically updated via
-- some function (e.g. slightly wrong.)
data Description = PlainDescription Text | DynamicDescription (forall w. w -> Entity -> Text)
-- | for overloadedstrings
instance IsString Description where
    fromString = PlainDescription . fromString

-- | also for overloadedstrings
instance Semigroup Description where
    (<>) (PlainDescription e) (PlainDescription e2) = PlainDescription (e <> e2)
    (<>) (PlainDescription e) (DynamicDescription e2) = DynamicDescription (\w e1 -> e <> e2 w e1)
    (<>) (DynamicDescription e2) (PlainDescription e)  = DynamicDescription (\w e1 -> e2 w e1 <> e)
    (<>) (DynamicDescription e) (DynamicDescription e2) = DynamicDescription (\w e1 -> e w e1 <> e2 w e1)
-- | we define a show instance just for debug purposes.
instance Show Description where
    show (PlainDescription t) = show t
    show (DynamicDescription _) = "dynamic description"

-- | generate a new entity ID
newEntity :: HasGameInfo u w => System u Int
newEntity = gameInfo . entityCounter <<%= (+1)

tryAction :: (HasWorld' w) => Text -> [Entity] -> State w (Maybe Bool)
tryAction action args = do
    w <- get
    let ac = Map.lookup action $ w ^. world . gameInfo . actions
    when (isNothing ac) (sayDbgLn $ "couldn't find the action " <> action)
    maybe (return $ Just False) (\(CompiledAction e) -> e args) ac

-- it's common so it can be saved for when play begins.
lookingActionName :: Text
lookingActionName = "looking"

makeLenses ''UncompiledRulebook