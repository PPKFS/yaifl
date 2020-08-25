module Yaifl.Activities
(
    makeActivity', makeActivity,
    doActivity', doActivity,
    printingNameOfADarkRoomName, printingDescriptionOfADarkRoomName,
    describingLocaleActivityName,
    printName', printName,
    capitalThe
) where

import Relude
import Yaifl.Say
import Yaifl.Utils
import Yaifl.Common
import Yaifl.Components
import Yaifl.Rulebooks
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as Map
import qualified Text.Show
import Data.List
import Control.Lens

makeActivity' :: Text -> RuleEvaluation (w, p) -> UncompiledActivity w () p
makeActivity' n r = makeActivity n (const ()) (zoom (alongside id _1) r)

makeActivity :: Text -> (w -> r) -> RuleEvaluation (w, (p, r)) -> UncompiledActivity w r p
makeActivity n iniActivity r = Activity n iniActivity 
                    (\p -> makeRulebook ("before " <> n) p []) 
                    (\r1 -> makeRulebook ("for " <> n) r1 [("", r)]) 
                    (\p -> makeRulebook ("before " <> n) p []) 

doActivity' :: (HasWorld u w) => Text -> System u ()
doActivity' n = doActivity n []

doActivity :: HasWorld u w => Text -> [Entity] -> System u ()
doActivity n params = do
    sayDbgLn $ "running activity " <> n
    w <- get
    maybe (sayDbgLn $ "couldn't find activity " <> n) (\(CompiledActivity x) -> do
        _ <- zoom world $ x params
        pass) (w ^. activities . at n)

printingNameOfADarkRoomName :: Text
printingNameOfADarkRoomName = "printing the name of a dark room activity"
printingNameOfADarkRoomImpl :: HasMessageBuffer w => UncompiledActivity w () ()
printingNameOfADarkRoomImpl = makeActivity' printingNameOfADarkRoomName (do
            say "Darkness"
            return Nothing)

printingDescriptionOfADarkRoomName :: Text
printingDescriptionOfADarkRoomName = "printing the description of a dark room activity"
printingDescriptionOfADarkRoomImpl :: HasMessageBuffer w => UncompiledActivity w () ()
printingDescriptionOfADarkRoomImpl = makeActivity' printingDescriptionOfADarkRoomName (do
            sayLn "It is pitch dark, and you can't see a thing."
            return Nothing)

printingNameOfSomethingName :: Text
printingNameOfSomethingName = "printing the name of something activity"
printingNameOfSomethingImpl :: HasComponent w w Object => UncompiledActivity w () Entity
printingNameOfSomethingImpl = makeActivity' printingNameOfSomethingName (do
    (w, e) <- get
    say . _name $ getComponent' w objectComponent e
    return $ Just True)

data SayOptions = NoOptions | SayOptions Article Capitalisation
data Article = Indefinite | Definite
data Capitalisation = Capitalised | Uncapitalised

noSayOptions :: SayOptions
noSayOptions = NoOptions

capitalThe :: SayOptions
capitalThe = SayOptions Definite Capitalised

printName' :: (HasComponent u w Object, HasID o) => o -> System u ()
printName' o = printName o noSayOptions

printName :: (HasComponent u w Object, HasID o) => o -> SayOptions -> StateT u Identity ()
printName o p = let pr = doActivity printingNameOfSomethingName [objID o] in
    case p of
        NoOptions -> pr 
        SayOptions Indefinite Capitalised -> do say "A "; pr
        SayOptions Definite Capitalised -> do say "The "; pr
        SayOptions Indefinite Uncapitalised -> do say "a "; pr
        SayOptions Definite Uncapitalised -> do say "the "; pr

printingLocaleParagraphAboutActivityName :: Text
printingLocaleParagraphAboutActivityName = "printing locale paragraph about activity"

describingLocaleActivityName :: Text
describingLocaleActivityName = "printing the locale description of something"
newtype LocaleDescription = LocaleDescription Int
describingLocaleActivityImpl :: HasStd w w => UncompiledActivity w LocaleDescription Entity
describingLocaleActivityImpl = Activity describingLocaleActivityName (const $ LocaleDescription 0) 
        (\r1 -> makeRulebook "" r1 [
                makeRule "initialise locale description rule" 
                    (do world . gameInfo . localePriorities .= Map.empty; return Nothing),
                makeRule "find notable objects rule" (do
                    doActivity choosingNotableLocaleObjectsActivityName [fst r1]
                    return Nothing)])
        (\r1 -> makeRulebook "" r1 [
                makeRule "interesting locale paragraphs rule" (do
                    lp <- use $ world . gameInfo . localePriorities
                    let sorted = Relude.sortBy (\(_, a) (_, b) -> compare a b) (Map.toAscList lp)
                    mapM_ (doActivity printingLocaleParagraphAboutActivityName . one . fst) sorted
                    return Nothing
                    ),
                makeRule "you can also see rule" (do
                    (w, (p, LocaleDescription e)) <- get
                    -- lp is everything that has a locale priority
                    let lp = Map.keys $ Map.filter (>0) (w ^. gameInfo . localePriorities) 
                    -- we then partition the list into things that have been mentioned and those which have not
                        (ment, notMent) = partition (_mentioned . getComponent' w physicalComponent) lp
                        sayNm x = do say x; printName p (SayOptions Definite Uncapitalised); say " you "
                        setMarked e' b = component' physicalComponent e' . markedForListing .= b
                    mapM_ (`setMarked` False) ment
                    mapM_ (`setMarked` True) notMent
                    -- so we only care about things marked for listing...which should be, at this point,
                    -- only things in the not mentioned list.
                    unless (null notMent) (do
                        if  | isComponent w roomComponent p -> if p == playerLocation' w
                                then say "You " 
                                else sayNm "In "
                            -- TODO: replace the second supporter with animal
                            | isComponent w supporterComponent p || isComponent w supporterComponent p -> sayNm "On "
                            | otherwise -> sayNm "In "
                        say "can "
                        when (e > 0) $ say "also "
                        say "see "
                        --let vs = fmapToSnd (getComponent w physicalComponent) lp
                        -- now what we want is to make a bunch of partitions of the list of marked for listing things
                        -- depending on whether they have a common holder
                        -- inform has yet another "if it's mentioned..." check that seems completely unnecessary.
                        pass
                        )
                    return Nothing
                    )
            ])
        (\r1 -> makeRulebook "" r1 [])

choosingNotableLocaleObjectsActivityName :: Text
choosingNotableLocaleObjectsActivityName = "choosing notable locale objects activity"

choosingNotableLocaleObjectsActivityImpl :: HasStd w w => UncompiledActivity w () Entity
choosingNotableLocaleObjectsActivityImpl = makeActivity' choosingNotableLocaleObjectsActivityName (do
    (w, e) <- get    
    let r = getComponent w enclosingComponent e
    whenJust r (mapM_ (\e' -> do
        gameInfo . localePriorities . at e' ?= 5
        component' physicalComponent e' . mentioned .= False) . _encloses)
    return Nothing
    )

{-
--there's something about making sure they can be pluralised which I'm ignoring
--if it has children and won't make the parser recurse?
--the parser will recurse if "always" is true (idfk) or if it's a supporter
--or if it's an open/transparent container.
--if one is worn or is giving off light and the other isn't
--if one's a container and the other isn't and the containers are different
groupingEquivalenceRelation :: HasStd u w => u -> Entity -> Entity -> Bool
groupingEquivalenceRelation u e1 e2 = 
    if | hasChildren u e1 && willRecurse u e1 -> False
       | hasChildren u e2 && willRecurse u e2 -> False
       --something about having omit contents from listing here
       | isWorn u e1 /= isWorn u e2 -> False
       | isEmittingLight u e1 /= isEmittingLight u e2 -> False
       | isMatchingContainers u e1 e2 -> False
       | otherwise -> True

hasChildren :: (HasWorld w w, HasWorld u w, Has w Enclosing) => u -> Entity -> Bool
hasChildren u = ifMaybe (not . null . _encloses) . getComponent u enclosingComponent

willRecurse :: (HasWorld u w, HasWorld w w, Has w Supporter, Has w Openable, Has w Container) => u -> Entity -> Bool
willRecurse u e1 = isComponent u supporterComponent e1 || 
                        ifMaybe (\e -> _opacity e == Transparent || 
                                    ifMaybe (== Open) (getComponent u openableComponent e1))
                                        (getComponent u containerComponent e1)

isWorn :: (HasWorld w w, HasWorld u w, Has w Physical) => u -> Entity -> Bool
isWorn u = ifMaybe (\e -> Wearable == _wearable e) . getComponent u physicalComponent

isEmittingLight :: (HasWorld w w, HasWorld u w, Has w Physical) => u -> Entity -> Bool
isEmittingLight u = ifMaybe (\e -> Lit == _lit e) . getComponent u physicalComponent

isMatchingContainers :: (HasWorld u w, HasWorld w w, Has w Openable) => u -> Entity -> Entity -> Bool
isMatchingContainers u e1 e2 = ifMaybe (\e1' -> ifMaybe (e1' ==) (getComponent u openableComponent e2)) 
            (getComponent u openableComponent e1)

combineStuff [] _ _ ys rj = pass
combineStuff ((_, Nothing):xs) h f ys rj = pass
{-
combineStuff ((e, Just p):xs) h f ys rj = if (_enclosedBy p) \= h 
    then 
        if isNothing h
            then combineStuff xs (_enclosedBy p) f ((e, Just p):ys) rj
            else combineStuff xs (_enclosedBy p) False ys ((e, Just p):rj)
    else 
-}
-}