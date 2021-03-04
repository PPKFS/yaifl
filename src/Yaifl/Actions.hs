module Yaifl.Actions
(
  addBaseActions
) where

import Yaifl.Prelude
import Yaifl.Common
import Yaifl.Rulebooks
import Yaifl.Components
import Yaifl.Activities
import Colog
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY


addAction :: (Show v, Monad m) => Action w v m -> World w m ()
addAction ac = do
    actionStore . at (_actionName ac) ?= BoxedAction ac

addBaseActions :: (HasContainer w, HasStore w Physical, Monad m) => World w m ()
addBaseActions = do
    actionProcessing .= defaultActionProcessingRules
    addAction lookingActionImpl

makeAction :: Monad m => Text -> Int -> [Rule w () m RuleOutcome] ->
    [Rule w () m RuleOutcome] -> [Rule w () m RuleOutcome] ->
    [Rule w () m RuleOutcome] -> Action w () m
makeAction n app bef chec carr repor = Action n [n] app (\_ -> makeRulebook "set action variables rulebook" [Rule "set action variables" (return $ Just ())])
                        (const $ makeRulebook "before action rulebook" bef)
                        (const $ makeRulebook "check action rulebook" chec)
                        (const $ makeRulebook "carry out action rulebook" carr)
                        (const $ makeRulebook "report action rulebook" repor)

data LookingActionVariables = LookingActionVariables
    {
        _visibilityCount :: Int,
        _visibilityLevels :: [Entity],
        _roomDescribingAction :: Text
    } deriving Show

lookingActionName = "looking"

lookingActionImpl :: (HasContainer w, HasStore w Physical, Monad m) => Action w LookingActionVariables m
lookingActionImpl = Action lookingActionName [lookingActionName] 0 (const lookingActionSet) (makeRulebookWithVariables "before looking rulebook" []) (makeRulebookWithVariables "check looking rulebook" []) carryOutLookingRules (makeRulebookWithVariables "report looking rulebook" [])

lookingActionSet :: (HasContainer w, HasStore w Physical, Monad m) => Rulebook w () m LookingActionVariables
lookingActionSet = makeRulebook "set action variables rulebook" [
        Rule "determine visibility ceiling rule" (do
        --TODO - set properly?
        actorID <- getActor
        actorLocation <- getLocation actorID
        vl <- traverse getVisibilityLevels actorLocation
        lightLevels <- recalculateLightOfParent actorID
        logDebug $ show vl
        return $ fmap (\x -> LookingActionVariables lightLevels (take lightLevels x) lookingActionName) vl)
    ]

recalculateLightOfParent :: Monad m => Entity -> World w m Int
recalculateLightOfParent _ = return 0

getVisibilityLevels :: (HasContainer w, HasStore w Physical, Monad m) => Entity -> World w m [Entity]
getVisibilityLevels e = do
    a <- findVisibilityHolder e
    case a of
        Nothing -> return []
        (Just a') -> if a' == e then return [e] else (do
            logDebug $ "Visibility holder of " <> show e <> " was " <> show a'
            a'' <- getVisibilityLevels a'
            return $  a' : a'')

findVisibilityHolder :: (HasContainer w, HasStore w Physical, Monad m) => Entity -> World w m (Maybe Entity)
findVisibilityHolder e' = do
    e_room <- e' `isType` "room"
    e_cont <- isOpaqueClosedContainer e'
    enclosing <- getComponent @Physical e'
    -- the visibility holder of a room or an opaque, closed container is itself
    -- otherwise, the enclosing entity
    if e_room || e_cont then return (Just e') else return $ _enclosedBy <$> enclosing

carryOutLookingRules :: (HasStore w Physical, Monad m) => LookingActionVariables -> Rulebook w LookingActionVariables m RuleOutcome
carryOutLookingRules = makeRulebookWithVariables "carry out looking rulebook" 
    [RuleWithVariables "room description heading rule" (do
            lift $ setStyle (Just PPTTY.bold)
            (LookingActionVariables cnt lvls _) <- getRulebookVariables
            let visCeil = viaNonEmpty last lvls 
            loc <- lift $ getActor >>= getLocation
            if | cnt == 0 -> lift $ doActivity printingNameOfADarkRoomName --no light, print darkness
               | isNothing visCeil -> logError "no visibility ceiling???"
               | visCeil == loc -> lift $ printName ceil --if the ceiling is the location, then print [the location]
               | True -> lift $ printNameEx ceil  --otherwise print [The visibility ceiling]
            
            mapM_ foreachVisibilityHolder (drop 1 lvls)
            lift $ sayLn ""
            lift $ setStyle Nothing
            --TODO: "run paragraph on with special look spacing"?
            return Nothing)]{-

forEachVisibilityHolder :: Entity -> RuleVarsT LookingActionVariables (World w m) b0
forEachVisibilityHolder = error "not implemented"
        makeRule "room description body rule" (do
            (w, (_, LookingActionVariables ceil lvl ac)) <- get
            let gi = w ^. gameInfo
                loc = playerLocation' w
                abbrev = _roomDescriptions gi == AbbreviatedRoomDescriptions
                someAbbrev = _roomDescriptions gi == SometimesAbbreviatedRoomDescriptions
            if | lvl == 0 -> unless (abbrev || (someAbbrev && _darknessWitnessed gi)) 
                    (do _ <- doActivity' printingDescriptionOfADarkRoomName ; pass)
               | ceil == loc -> 
                    unless (abbrev || (someAbbrev && ac /= lookingActionName))
                            ((sayLn . getDescription' w) (getComponent' w objectComponent loc))
               | True -> pass
            return Nothing),
        makeRule "room description paragraphs about objects rule" (do
            (w, (_, LookingActionVariables _ lvl _)) <- get
            let lvls = visibilityLvls lvl w (getPlayer' w)
            when (lvl > 0) (mapM_ (`whenJust` (\e' -> do doActivity describingLocaleActivityName [e']; pass)) lvls
                )
            return Nothing)]-}
        
foreachVisibilityHolder :: Entity -> RuleVarsT LookingActionVariables (World w m) ()
foreachVisibilityHolder e = do
            sup <- lift $ e `isType` "supporter"
            lift $ if sup then say "(on " else say "(in "
            lift $ printName e
            lift $ say ")"