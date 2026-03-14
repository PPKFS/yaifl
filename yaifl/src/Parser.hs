{-# LANGUAGE RecordWildCards #-}
module Parser where

import Yaifl.Prelude
import Data.Aeson
import Data.Yaml
import Data.Aeson.KeyMap as KM hiding (map, toList)
import qualified Data.List as L
import qualified Data.Aeson.Key as KM
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Aeson.KeyMap as KM hiding (map)
newtype SkillName = SkillName Text
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, FromJSON, FromJSONKey, Semigroup, IsString)

newtype UnlockName = UnlockName Text
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, FromJSON, FromJSONKey)

newtype SkillLevel = SkillLevel Int
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, FromJSON, FromJSONKey, Num, Enum, Bounded, Real, Integral)

data Skill = Skill
  { name :: SkillName
  , unlocks :: [SkillTask] --M.Map UnlockName Unlock
} deriving stock (Eq, Show, Ord, Generic)

type Unlock = Value

type QuestName = Text
data TaskRequirement = SkillRequirement SkillName SkillLevel | QuestRequirement QuestName
  deriving stock (Eq, Show, Ord, Generic)
data SkillTask = Task
  { skillTaskName :: UnlockName
  , produces :: UnlockName
  , description :: Text
  , requires :: [TaskRequirement]
  , skills :: S.Set SkillName
  , commonFields :: Maybe Value
  --, items :: Maybe Value
  } deriving stock (Eq, Show, Ord, Generic)

data PartialSkillTask = PartialSkillTask
  { descriptionPatch :: Maybe Text
  , requiresPatch :: [TaskRequirement]
  , skillsPatch :: [SkillName]
  --, itemsPatch :: Maybe [Value]
  } deriving stock (Eq, Show, Ord, Generic)

applyPartialSkill :: PartialSkillTask -> SkillTask -> SkillTask
applyPartialSkill PartialSkillTask{..} sk =
  sk
  & #description %~ maybe id const descriptionPatch
  & #requires %~ (<> requiresPatch)
  & #skills %~ S.union (S.fromList skillsPatch)
  -- & #items %~ (<> itemsPatch)

parseTaskEntry :: SkillName -> Value -> Parser [SkillTask]
parseTaskEntry skillName = withObject "task entry" $ \t -> do
  case (t !? "task", t !? "variant" , t !? "group") of
    (Just (String taskName), Nothing, Nothing) -> one <$> parseSkillTask skillName (UnlockName taskName) t
    (Nothing, Just (String variantName), Nothing) -> parseVariantGroup skillName variantName t
    (Nothing, Nothing, Just (String groupName)) -> parseTaskGroup skillName (UnlockName groupName) t
    _ -> fail $ "expected a task, variant, or group string-valued key and instead got " <> show t

newtype ListOrSingletonName = LOS [Text]

instance FromJSON ListOrSingletonName where
  parseJSON v = case v of
    String x -> pure $ LOS [x]
    Array xs -> LOS . toList <$> mapM parseJSON xs
    x -> fail $ "expected either a shorthand single entry list of strings, or a list of strings and got " <> show x
data GroupNaming = Suffix | Prefix | None | Always Text
  deriving stock (Show, Eq, Ord, Generic)

instance FromJSON GroupNaming where
  parseJSON = withText "GroupNaming" $ \case
    "suffix" -> return Suffix
    "prefix" -> return Prefix
    "none" -> return None
    x
      | Just n <- T.stripPrefix "always " x -> return (Always n)
    x -> fail (show x)

data GroupInfo = GroupInfo GroupNaming SkillTask

data TaskGroupItems = TGItems [Object] | TGJewellery [Object] | TGWood [Object]
  deriving stock (Eq, Ord, Show)

parseTaskGroup :: SkillName -> UnlockName -> Object -> Parser [SkillTask]
parseTaskGroup skillName groupName o = do
  naming <- fromMaybe Prefix <$> o .:? "naming"
  let skillfmt = unwords [show skillName, ":", show groupName]
  desc :: Text <- lookupRequiredField skillfmt o "desc"
  commonFields :: Maybe Value <- o .:? "common"
  items :: Maybe ListOrSingletonName <- o .:? "items"
  (LOS quests) :: ListOrSingletonName <- fromMaybe (LOS []) <$> o .:? "quests"
  let groupInfo = GroupInfo naming $ Task
        { skillTaskName = groupName
        , produces = groupName -- junk
        , description = desc
        , requires = map QuestRequirement quests
        , skills = S.fromList [skillName]
        , commonFields
        -- , items
        }
  -- >>= mapM (groupItem skillName groupInfo)
  itemList <- (TGItems <$> lookupRequiredField ("the items of a task group " <> show groupName) o "entries")
    <|> (TGJewellery <$> lookupRequiredField ("the jewellery of a task group " <> show groupName) o "jewellery")
    <|> (TGWood <$> lookupRequiredField ("the wood of a task group " <> show groupName) o "wood" )
    <|> fail (toString $ unwords ["in a task group, could not find a list of tasks (e.g. entries, jewellery, wood) ", show o])
  case itemList of
    TGItems vals -> mapM (groupItem skillName groupInfo) vals
    TGJewellery vals -> mapM (groupItem skillName groupInfo) vals
    TGWood vals -> mapM (groupItem skillName groupInfo) vals

groupItem :: SkillName -> GroupInfo -> Object -> Parser SkillTask
groupItem skillName (GroupInfo gn gi) obj =
  let mkName k =
        UnlockName (case gn of
          Prefix -> unwords [coerce $ gi ^. #skillTaskName, KM.toText k]
          Suffix -> unwords [KM.toText k, coerce $ gi ^. #skillTaskName]
          Always x -> unwords [KM.toText k, x]
          None -> KM.toText k)
  in case KM.toList obj of
    -- one, shorthand
    [(k, Number lvl)] ->
      let n = mkName k
      in return $ gi
      & #skillTaskName .~ n
      & #produces .~ n
      & #requires %~ (SkillRequirement skillName (round lvl):)
      & #description %~ T.replace "<item>" (coerce n)
    -- one, longhand
    _ -> do
      name <- lookupRequiredField (unwords ["name of a longform group item; perhaps you wrote it in shorthand - name: <level>?", show gi]) obj "name"
      level <- lookupRequiredField (unwords ["level of a longform group item", show gi]) obj "level"
      items :: Maybe ListOrSingletonName <- obj .:? "items"
      let n = mkName name
      return $ gi
        & #skillTaskName .~ n
        & #produces .~ n
        & #requires %~ (SkillRequirement skillName level:)
        & #description %~  T.replace "<item>" (coerce n)


-- -- a single entry
-- o .: "items" >>= (fmap (\x -> return [x]) . singleItem)
singleItem :: Text -> Parser Value
singleItem _ = fail "oop"

lookupRequiredField :: FromJSON a => Text -> Object -> Key -> Parser a
lookupRequiredField txt o k = do
  mbV <- o .:? k
  maybe (fail $ toString $ unwords ["missing required field ", show k, " in ", txt]) pure mbV

parseSkillTask :: SkillName -> UnlockName -> Object -> Parser SkillTask
parseSkillTask skillName taskName o = do
  let skillfmt = unwords [show skillName, ":", show taskName]
  desc <- lookupRequiredField skillfmt o "desc"
  produces <- fromMaybe taskName <$> o .:? "product"
  commonFields :: Maybe Value <- o .:? "common"
  items :: Maybe Value <- o .:? "items"
  level <- lookupRequiredField skillfmt o "level"
  otherSkills :: Maybe Value <- o .:? "skills"
  return $ Task
    { description = desc
    , produces
    , skillTaskName = taskName
    , requires = [SkillRequirement skillName level], skills = S.fromList [skillName], commonFields } -- , items }

parseVariantGroup :: SkillName -> Text -> Object -> Parser [SkillTask]
parseVariantGroup _ _ _ = return []

type TaskGroup = Value
type VariantGroup = Value

instance FromJSON Skill where
  parseJSON = withObject "Skill" $ \o -> do
    name <- o .: "name"
    unlocks <- o .: "unlocks"
    f2p <- maybe (return []) (fmap mconcat . mapM (parseTaskEntry name)) (unlocks !? "f2p")
    members <- maybe (return []) (fmap mconcat . mapM (parseTaskEntry name)) (unlocks !? "members")
    return $ Skill
      { name = name
      , unlocks = f2p <> members
      }
      --M.unionWith (\i -> error ("found a duplicate between f2p and members unlocks at " <> show i)) f2p members }

decodeSkills :: HasCallStack => IO Skill
decodeSkills = either (error . show) L.head <$> decodeFileEither @[Skill] "skills.yaml"

prettyPrintSkills :: Skill -> IO ()
prettyPrintSkills Skill{..} = do
  print $ "Name: " <> name
  putStrLn "Unlocks: "
  mapM_ (\Task{..} -> print $ unwords [description, show requires] ) unlocks