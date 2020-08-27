module Yaifl.Actions
(
    LookingActionVariables(..), compileAction,
    actionProcessingRulebookImpl, lookingActionImpl
) where

import Relude
import Yaifl.Say
import Yaifl.Common
import Yaifl.Rulebooks
import Yaifl.Components
import Yaifl.Activities
import Control.Lens
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY

actionProcessingRulebookName :: Text
actionProcessingRulebookName = "action processing rulebook"

actionProcessingRulebookImpl :: (HasMessageBuffer w, ActionArgs p, RulebookArgs r) => 
                UncompiledAction w r p -> [Entity] -> UncompiledRulebook w (p, r)
actionProcessingRulebookImpl actionRules e = case unboxArguments e of
    Nothing -> makeRulebook actionProcessingRulebookName (defaultActionArguments, defaultArguments) [] --todo, nicer?
    Just p -> makeRulebook actionProcessingRulebookName (p, defaultArguments) [
        makeRule "set action variables rule" (do
            (_, (p', _)) <- get
            let arb = _setActionVariables actionRules p'
            _ <- getRule $ compileRulebook arb Full
            return Nothing),
        makeBlankRule "before stage rule",
        makeBlankRule "carrying requirements rule",
        makeBlankRule "basic visibility rule",
        makeBlankRule "instead stage rule",
        makeBlankRule "requested actions require persuasion rule",
        makeBlankRule "carry out requested actions rule",
        makeBlankRule "investigate player awareness rule",
        makeBlankRule "check stage rule",
        makeRule"carry out stage rule" (do
            (_, (p, r)) <- get
            getRule $ compileRulebook (_carryOutActionRules actionRules (p, r)) Full
            ),
        makeBlankRule "after stage rule",
        makeBlankRule "investigate player awareness after rule",
        makeBlankRule "report stage rule",
        makeBlankRule "clean actions rule",
        makeBlankRule "end action processing rule"]
    
makeAction :: RulebookArgs r => Text -> [(Text, RuleEvaluation (w, (a, r)))] -> 
    [(Text, RuleEvaluation (w, (a, r)))] -> [(Text, RuleEvaluation (w, (a, r)))] -> 
    [(Text, RuleEvaluation (w, (a, r)))] -> [(Text, RuleEvaluation (w, (a, r)))] -> UncompiledAction w r a
makeAction n s b ch ca rep = Action n [n] (\p -> 
                        makeRulebook "set action variables rulebook" (p, defaultArguments) s)
                        (\r -> makeRulebook "before action rulebook" r b)
                        (\r -> makeRulebook "check action rulebook" r ch)
                        (\r -> makeRulebook "carry out action rulebook" r ca)
                        (\r -> makeRulebook "report action rulebook" r rep)

compileAction ::HasStd' w => UncompiledAction w r p
                                 -> (UncompiledAction w r p -> [Entity] ->  UncompiledRulebook w (p, r)) -> Action w
compileAction action aprules = CompiledAction (\e -> do
    sayDbgLn $ "doing action " <> _actionName action
    let (CompiledRulebook r) = compileRulebook' (aprules action e) Full
    r)

data LookingActionVariables = LookingActionVariables
    {
        _visibilityCeiling :: Entity,
        _visibilityLevel :: Int,
        _roomDescribingAction :: Text
    } deriving Show

instance RulebookArgs LookingActionVariables where
    defaultArguments = LookingActionVariables (-1) 0 lookingActionName

foreachVisibilityHolder :: HasComponent u w Supporter => Maybe Entity -> System u ()
foreachVisibilityHolder Nothing = error "you broke it."
foreachVisibilityHolder (Just e) = do
            w <- get
            if isComponent w supporterComponent e 
            then do say "(on "; printName' e; say ")"
            else do say "(in "; printName' e; say ")" 

lookingVarLens :: Lens' (a, (b, c)) c
lookingVarLens = _2 . _2

findVisibilityHolder :: HasStd u w => u -> Maybe Entity -> Maybe Entity
findVisibilityHolder _ Nothing = Nothing
findVisibilityHolder w (Just e) = if isARoom e || isAContainer e then Nothing else parentOf e where
    --if the entity is an opaque, closed container OR a room it's nothing
    -- of course if the entity is a room we should never need to call this?
        isARoom = isComponent w roomComponent
        isAContainer = do
            cont <- getComponent w containerComponent
            op' <- getComponent w openableComponent
            return $ (_opacity <$> cont) == Just Opaque && op' == Just Closed
        parentOf v = _enclosedBy <$> getComponent w physicalComponent v

visibilityLvls :: HasStd u w => Int -> u -> Entity -> [Maybe Entity]
visibilityLvls lvl w player = take lvl $ drop 1 $ iterate (findVisibilityHolder w) (Just player)

lookingActionImpl :: HasStd w w => UncompiledAction w LookingActionVariables () 
lookingActionImpl = makeAction lookingActionName 
    [
        makeRule "determine visibility ceiling rule" (do
        (w, _) <- get
        --TODO - set properly?
        lookingVarLens .= LookingActionVariables (playerLocation' w) 1 lookingActionName
        return Nothing)
    ] [] [] 
    [makeRule "room description heading rule" (do
            setStyle (Just PPTTY.bold)
            (w, (_, LookingActionVariables ceil lvl _)) <- get
            let loc = playerLocation' w
                player = getPlayer' w
            if | lvl == 0 -> doActivity' printingNameOfADarkRoomName
               | ceil == loc -> printName' ceil
               | True -> printName ceil capitalThe
            mapM_ foreachVisibilityHolder $ drop 1 $ visibilityLvls lvl w player
            sayLn ""
            setStyle Nothing
            --TODO: "run paragraph on with special look spacing"?
            return Nothing),
        makeRule "room description body rule" (do
            (w, (_, LookingActionVariables ceil lvl ac)) <- get
            let gi = w ^. gameInfo
                loc = playerLocation' w
                abbrev = _roomDescriptions gi == AbbreviatedRoomDescriptions
                someAbbrev = _roomDescriptions gi == SometimesAbbreviatedRoomDescriptions
            if | lvl == 0 -> unless (abbrev || (someAbbrev && _darknessWitnessed gi)) 
                    (do doActivity' printingDescriptionOfADarkRoomName ; pass)
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
            return Nothing)] []