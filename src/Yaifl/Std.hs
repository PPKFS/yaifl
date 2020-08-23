{- |
Copyright: (c) 2020 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <thecommunistduck@hotmail.co.uk>

Yet another interactive fiction library.
-}

module Yaifl.Std 
(
    Player, Physical, RoomData, Enclosing, Container, Openable, Supporter,
    makePlayer, makeThing, makeThingWithoutDescription, makeRoom,
    whenPlayBeginsRulesImpl, printingNameImpl, HasStd, HasStd',
    printName,
    introText,
    whenPlayBeginsName, printNameName, printDarkRoomNameName, printingNameOfADarkRoomImpl,
    actionProcessingRulebookImpl,
    LookingActionVariables, lookingActionImpl, lookingActionName,
    defaultWorld
) where

import Relude
import Data.String
import Yaifl.Common
import Yaifl.Say
import Yaifl.Utils
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY
import qualified Language.Haskell.TH as TH
import qualified Data.Text as Text
import qualified Data.Set as DS
import Control.Lens

{- TYPES -}

type HasStd a w = (HasWorld a w, HasWorld w w, HasGameInfo a w, HasGameInfo' w, Has w Object, Has w Physical, 
                    Has w Player, Has w Enclosing, Has w RoomData, 
                    Has w Container, Has w Openable, Has w Supporter,
                    HasMessageBuffer w, HasMessageBuffer a)
type HasStd' w = HasStd w w

data ThingLit = Lit | Unlit deriving Show
data Edibility = Edible | Inedible deriving Show
data Portability = FixedInPlace | Portable deriving Show
data Wearability = Wearable | Unwearable deriving Show
data Pushability = PushableBetweenRooms | NotPushableBetweenRooms deriving Show

data Physical = Physical
    {
        _location :: Entity,
        _lit :: ThingLit,
        _edible :: Edibility,
        _portable :: Portability,
        _wearable :: Wearability,
        _pushable :: Pushability,
        _enclosedBy :: Maybe Entity
    } deriving Show

physicalComponent :: Proxy Physical
physicalComponent = Proxy
emptyPhysical :: Physical
emptyPhysical = Physical missingRoom Lit Edible Portable Wearable PushableBetweenRooms (Just missingRoom)

makeThing :: HasStd a w => Text -> Description -> System a Entity
makeThing n d = do
    e <- makeObject n d
    addComponent e emptyPhysical
    return e

makeThingWithoutDescription :: HasStd a w => Text -> System a Entity
makeThingWithoutDescription n = makeThing n ""

data Darkness = Lighted | Dark deriving Show
data IsVisited = Visited | Unvisited deriving Show
type MapConnections = Store Entity
type ContainingRegion = Maybe Entity

data RoomData = RoomData
    {
        _isVisited :: IsVisited,
        _mapConnections :: MapConnections,
        _containingRegion :: ContainingRegion
    } deriving Show

roomComponent :: Proxy RoomData
roomComponent = Proxy :: (Proxy RoomData)

makeRoom :: HasStd a w => Text -> System a Entity
makeRoom n = do
    e <- makeObject n ("It's the " <> PlainDescription n <> ".")
    addComponent e (RoomData Unvisited emptyStore Nothing)
    addComponent e (Enclosing DS.empty)
    return e

getFirstRoom :: HasGameInfo a w => a -> Maybe Entity
getFirstRoom w = w ^. gameInfo . firstRoom

newtype Enclosing = Enclosing
    {
        _encloses :: Set Entity
    } deriving Show
enclosingComponent :: Proxy Enclosing
enclosingComponent = Proxy

data Container = Container
    {
        _enterable :: Bool,
        _opacity :: Opacity,
        _carryingCapacity :: Int
    } deriving Show

data Supporter = Supporter
    {
        _supporterEnterable :: Bool,
        _supporterCarryingCapacity :: Int
    } deriving Show
data Opacity = Opaque | Transparent deriving (Eq, Show)

containerComponent :: Proxy Container
containerComponent = Proxy :: (Proxy Container)

supporterComponent :: Proxy Supporter
supporterComponent  = Proxy :: (Proxy Supporter)

newtype Player = Player Int deriving Show

data Openable = Open | Closed deriving (Eq, Show)

openableComponent :: Proxy Openable
openableComponent = Proxy :: (Proxy Openable)

playerComponent :: Proxy Player
playerComponent = Proxy :: Proxy Player

makePlayer :: HasStd a w => System a Entity
makePlayer = do
    e <- makeThing "yourself" "it's you."
    addComponent globalComponent (Player e)
    return e

getPlayer :: (HasWorld s w, Has w Player) => s -> Maybe Entity
getPlayer w = coerce <$> (w ^. world . store (Proxy :: Proxy Player) . at globalComponent)

playerLocation :: HasStd u w => u -> Maybe Entity
playerLocation u = _location <$> (getComponent u physicalComponent =<< getPlayer u)

makeLenses ''Physical
makeLenses ''Enclosing
defaultWorld :: [TH.Name]
defaultWorld = [''Object, ''RoomData, ''Physical, ''Enclosing, ''Player, ''Openable, ''Container, ''Supporter]

move :: HasStd a w => Entity -> Entity -> System a Bool
move obj  le = do
    w <- get
    let mp = getComponent w physicalComponent obj
        mloc = getComponent w enclosingComponent le
        mcurrLoc = mp ^? _Just . location
    doIfExists3 mp mloc mcurrLoc (show obj <> " no physical thing to move") "no future loc" "no current loc" 
        (\_ _ c -> do
            world . component physicalComponent obj . _Just . location .= le
            world . component physicalComponent obj . _Just . enclosedBy ?= c
            world . component enclosingComponent c . _Just . encloses %= DS.delete obj
            world . component enclosingComponent le . _Just . encloses %= DS.insert obj

            return True
        )

whenPlayBeginsRulesImpl :: (HasStd' w) => UncompiledRulebook w ()
whenPlayBeginsRulesImpl = (blankRulebook whenPlayBeginsName) {
    _rules = 
        [
            makeRule "display banner rule" (do
                sayIntroText
                return Nothing),
                
            makeRule "position player in model world rule" (do
                w <- get
                whenJust (liftA2 move (getPlayer w) (getFirstRoom w)) (\e -> do
                    _ <- e
                    pass)
                return Nothing),
            makeRule "initial room description rule" (do
                tryAction lookingActionName []
                return Nothing)
        ]
}

introText :: Text -> [Text]
introText w = [longBorder<>"\n", shortBorder <> " " <> w <> " " <> shortBorder<>"\n", 
                longBorder<>"\n\n"]
            where shortBorder = "------" 
                  totalLength = 2 * Text.length shortBorder + Text.length w + 2
                  longBorder = foldr (<>) "" $ replicate totalLength ("-" :: Text)

sayIntroText :: HasStd a w => System a ()
sayIntroText = do
    w <- get
    setStyle (Just (PPTTY.color PPTTY.Green <> PPTTY.bold))
    mapM_ say (introText $ w ^. gameInfo . title)
    setStyle Nothing
    pass

actionProcessingRulebookName :: Text
actionProcessingRulebookName = "action processing rulebook"

actionProcessingRulebookImpl :: (HasStd' w, ActionArgs p, RulebookArgs r) => UncompiledAction w r p -> [Entity] -> 
                                UncompiledRulebook w (p, r)
actionProcessingRulebookImpl actionRules e = (blankRulebookVars actionProcessingRulebookName ar) {
    _rules = [
        makeRuleWithArgs "set action variables rule" (do
            (w, (p, _)) <- get
            if isBad p then return (Just False) else (do
                let arb = _setActionVariables actionRules p
                _ <- getRule . compileRulebookWithResult $ arb
                return Nothing)
            ),
        makeRuleWithArgs "before stage rule" (do
            return Nothing
            ),
        makeBlankRule "carrying requirements rule",
        makeBlankRule "basic visibility rule",
        makeBlankRule "instead stage rule",
        makeBlankRule "requested actions require persuasion rule",
        makeBlankRule "carry out requested actions rule",
        makeBlankRule "investigate player awareness rule",
        makeBlankRule "check stage rule",
        makeRuleWithArgs "carry out stage rule" (do
            (_, (p, r)) <- get
            getRule . compileRulebookWithResult $ _carryOutActionRules actionRules (p, r)
            ),
        makeBlankRule "after stage rule",
        makeBlankRule "investigate player awareness after rule",
        makeBlankRule "report stage rule",
        makeBlankRule "clean actions rule",
        makeBlankRule "end action processing rule"
    ]
} where ar = (fromMaybe defaultArguments (unboxArguments e), defaultRulebookArguments)

printingNameOfADarkRoomImpl :: (HasMessageBuffer w) => UncompiledActivity w () ()
printingNameOfADarkRoomImpl = makeActivity printDarkRoomNameName (do
            say "Darkness"
            return Nothing)

printingDescriptionOfADarkRoomImpl :: (HasMessageBuffer w) => UncompiledActivity w () ()
printingDescriptionOfADarkRoomImpl = makeActivity printDarkRoomDescriptionName (do
            sayLn "It is pitch dark, and you can't see a thing."
            return Nothing)

printingNameImpl :: HasStd' w => UncompiledActivity w () Entity
printingNameImpl = makeActivity printNameName (do
    (w, (e, ())) <- get
    whenJust (getComponent w objectComponent e) (say . _name)
    return $ Just True)

printNameName :: Text
printNameName = "printing the name"
printDarkRoomNameName :: Text
printDarkRoomNameName = "printing the name of a dark room"
printDarkRoomDescriptionName :: Text
printDarkRoomDescriptionName = "printing the description of a dark room"
describingLocaleActivityName :: Text
describingLocaleActivityName = "printing the locale description of something"
whenPlayBeginsName :: Text
whenPlayBeginsName = "when play begins rules"

lookingActionName :: Text
lookingActionName = "looking"

data NameProperness = ImproperNamed | ProperNamed deriving Show
data NamePlurality = SingularNamed | PluralNamed deriving Show

data SayOptions = NoOptions | SayOptions Article Capitalisation
data Article = Indefinite | Definite
data Capitalisation = Capitalised | Uncapitalised

noSayOptions :: SayOptions
noSayOptions = NoOptions

capitalThe :: SayOptions
capitalThe = SayOptions Definite Capitalised
printName :: (HasStd u w, HasID o) => o -> System u ()
printName o = printNameEx o noSayOptions

printNameEx :: (HasStd u w, HasID o) => o -> SayOptions -> StateT u Identity ()
printNameEx o p = let pr = zoom world $ doActivityWithArgs printNameName [objID o] in
    case p of
        NoOptions -> pr 
        SayOptions Indefinite Capitalised -> do say "A "; pr
        SayOptions Definite Capitalised -> do say "The "; pr
        SayOptions Indefinite Uncapitalised -> do say "a "; pr
        SayOptions Definite Uncapitalised -> do say "the "; pr

doActivity :: HasStd u w => Text -> System u ()
doActivity n = doActivityWithArgs n []

doActivityWithArgs :: HasStd u w => Text -> [Entity] -> System u ()
doActivityWithArgs n params = zoom world $ do
    sayDbgLn $ "running activity " <> n
    w <- get
    maybe (sayDbgLn $ "couldn't find activity " <> n) (\(CompiledActivity x) -> do
        _ <- x params
        pass) (w ^. gameInfo . activities . at n)

data LookingActionVariables = LookingActionVariables
    {
        _visibilityCeiling :: Entity,
        _visibilityLevel :: Int,
        _roomDescribingAction :: Text
    } deriving Show

instance RulebookArgs LookingActionVariables where
    defaultRulebookArguments = LookingActionVariables missingRoom 0 lookingActionName

visrule :: HasStd' w => [(Text, RuleEvaluation (w, ((), LookingActionVariables))) ]
visrule = [makeRuleWithArgs "determine visibility ceiling rule" (do
        (w, _) <- get
        let loc = playerLocation w
            vis_lvl = 1
            roomAction = lookingActionName
        --TODO
        maybe (do 
                sayDbgLn "no player location found" 
                return $ Just False) 
            (\l -> do
                lookingVarLens .= LookingActionVariables l vis_lvl roomAction
                return Nothing) loc)]

lookingVarLens :: Lens' (a, (b, c)) c
lookingVarLens = _2 . _2

makeAction :: RulebookArgs r => Text -> [(Text, RuleEvaluation (w, (a, r)))] -> 
    [(Text, RuleEvaluation (w, (a, r)))] -> [(Text, RuleEvaluation (w, (a, r)))] -> 
    [(Text, RuleEvaluation (w, (a, r)))] -> [(Text, RuleEvaluation (w, (a, r)))] -> UncompiledAction w r a
makeAction n s b ch ca rep = Action n [n] (\p -> 
                        (blankRulebookVars "set action variables rulebook" (p, defaultRulebookArguments)) { _rules = s})
                        (\r -> (blankRulebookVars "before action rulebook" r) { _rules = b})
                        (\r -> (blankRulebookVars "check action rulebook" r) { _rules = ch})
                        (\r -> (blankRulebookVars "carry out action rulebook" r) { _rules = ca})
                        (\r -> (blankRulebookVars "report action rulebook" r) { _rules = rep})

lookingActionImpl :: HasStd' w => UncompiledAction w LookingActionVariables () 
lookingActionImpl = makeAction lookingActionName visrule [] [] [
    makeRuleWithArgs "room description heading rule" (let 
        foreachVisibilityHolder :: HasStd u w => Maybe Entity -> System u ()
        foreachVisibilityHolder Nothing = pass
        foreachVisibilityHolder (Just e) = do
            w <- get
            if isComponent w supporterComponent e 
            then do say "(on "; printName e; say ")"
            else do say "(in "; printName e; say ")" in 
        do
        setStyle (Just PPTTY.bold)
        v <- use lookingVarLens
        (w, _) <- get
        whenJust (playerLocation w) (\loc ->
            if | _visibilityLevel v == 0 -> doActivity printDarkRoomNameName 
               | _visibilityCeiling v == loc -> printName $ _visibilityCeiling v
               | True -> printNameEx (_visibilityCeiling v) capitalThe)
        mapM_ foreachVisibilityHolder $ take (_visibilityLevel v - 1) $ iterate (findVisibilityHolder w) $ getPlayer w
        sayLn ""
        setStyle Nothing
        --TODO: "run paragraph on with special look spacing"?
        return Nothing),
    makeRuleWithArgs "room description body rule" (do
        (w, (_, v)) <- get
        let gi = w ^. gameInfo
        sayDbgLn $ show v
        whenJust (playerLocation w) (\loc ->
            if | _visibilityLevel v == 0 ->
                    unless (_roomDescriptions gi == AbbreviatedRoomDescriptions || 
                         (_roomDescriptions gi == SometimesAbbreviatedRoomDescriptions && _darknessWitnessed gi))
                            (doActivity printDarkRoomDescriptionName)
                | _visibilityCeiling v == loc -> 
                    unless (_roomDescriptions gi == AbbreviatedRoomDescriptions || 
                         (_roomDescriptions gi == SometimesAbbreviatedRoomDescriptions 
                                && _roomDescribingAction v /= lookingActionName))
                            (whenJust (getComponent w objectComponent loc) (sayLn . getDescription w))
                | True -> pass)
        return Nothing
        ),
    makeRuleWithArgs "room description paragraphs about objects rule" (do
        (w, (_, v)) <- get
        when (_visibilityLevel v > 0) (
            mapM_ (`whenJust` (\e' -> doActivityWithArgs describingLocaleActivityName [e'])) 
                    $ take (_visibilityLevel v - 1) $ iterate (findVisibilityHolder w) $ getPlayer w
            )
        return Nothing)] []

findVisibilityHolder :: HasStd u w => u -> Maybe Entity -> Maybe Entity
findVisibilityHolder w e = if ifMaybe isARoom e || ifMaybe isAContainer e then Nothing else parentOf e where
    --if the entity is an opaque, closed container OR a room it's nothing
    -- of course if the entity is a room we should never need to call this?
        isARoom = isComponent w roomComponent
        isAContainer = do
            cont <- getComponent w containerComponent
            op' <- getComponent w openableComponent
            return $ (_opacity <$> cont) == Just Opaque && op' == Just Closed
        parentOf v = _enclosedBy =<< getComponent w physicalComponent =<< v

{-

function create_looking!(w::World)
    #abbbrv form allowed is "are we using go"

    #I have 0 clue what this code does but it's what the spec says.

    function check_arrival_rule(w::World, r::Rulebook)
        if r.variables[:visibility_level] == 0
            w.darkness_witnessed = true
        else
            visited(w, location(w.player))
        end
    end

    function other_people_looking(world::World, r::Rulebook)
        if actor(r) != id(world.player)
            print_name!(w, actor(r))
            sayln!(w, " looks around.")
        end
    end
    c = look.carry_out_rules
    add_rule!(c, :room_description_heading, desc_heading_rule, false)
    add_rule!(c, :room_description_body, desc_body_rule, false)
    add_rule!(c, :room_description_paragraphs_about_objects, desc_obj_rule, false)
    add_rule!(c, :check_new_arrivals, check_arrival_rule, false)
    add_rule!(look.report_rules, :other_people_looking, other_people_looking)

    look
end

-}