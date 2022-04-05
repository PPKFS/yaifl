{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams  #-}

module Yaifl.Types where

import Solitude
import Yaifl.Messages
import qualified Data.EnumSet as ES
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM

import qualified Data.Aeson as A
import qualified Data.Text.Lazy.Builder as B
import Katip.Format.Time (formatAsLogTime)

import qualified Data.Text as T
import Data.Aeson.Types ( ToJSON(toJSON) )

import Control.Monad.Except (MonadError)
import Yaifl.Objects.Object
import Yaifl.Common
-- | Whether a thing is inherently lit or not. This counts for lighting up spaces.


{-
instance Default Enclosing where
  blank = Enclosing ES.empty Nothing

instance Default ThingData where
  blank = ThingData defaultVoidID NotLit NotWearable Described

instance Default RoomData where
  blank = RoomData Unvisited Lighted blank Nothing blank


-}

{-





instance Default (Text -> Rule s v r) where
  blank n = Rule n (\v -> do
    warn $ bformat (stext %! " needs implementing") n
    return (v, Nothing))

--instance HasBuffer (World s) 'LogBuffer => Default (String -> Rule s v r) where
--  blank = blank . toText



instance Default (Text -> Rulebook s v v r) where
  blank n = Rulebook n Nothing (ParseArguments (return . Just)) []

blankRulebook ::
  Text 
  -> Rulebook s v v r
blankRulebook = blank


instance Default (Timestamp -> UnverifiedArgs s) where
  blank = UnverifiedArgs . Args Nothing []

instance Default (UnverifiedArgs s) where
  blank = UnverifiedArgs $ Args Nothing [] 0

-- | 'ActionRulebook's run over specific arguments; specifically, they expect
-- their arguments to be pre-verified; this allows for the passing of state.
type ActionRulebook s v = Rulebook s (Args s v) (Args s v) Bool




data Activity o v r = Activity
    { _activityName :: !Text
    , _activityDefault :: Maybe r
    , _activityBeforeRules :: !(Rulebook o v v ())
    , _activityCarryOutRules :: !(Rulebook o v v r)
    , _activityAfterRules :: !(Rulebook o v v ())
    }

data LocaleVariables s = LocaleVariables
  { _localePriorities :: LocalePriorities s
  , _localeDomain :: !(AnyObject s)
  , _localeParagraphCount :: Int
  }

-- | TODO
data ActivityCollection s = ActivityCollection
  { printingNameOfADarkRoom :: !(Activity s () ())
  , printingNameOfSomething :: !(Activity s (AnyObject s) ())
  , printingDescriptionOfADarkRoom :: !(Activity s () ())
  , choosingNotableLocaleObjects :: !(Activity s (AnyObject s) (LocalePriorities s))
  , printingLocaleParagraphAbout :: !(Activity s (LocaleVariables s, LocaleInfo s) (LocaleVariables s))
  , describingLocale :: !(Activity s (LocaleVariables s) ())
  }


data WorldModel s d v o = WorldModel
  { _things :: !(Store (AbstractThing s))
  , _rooms :: !(Store (AbstractRoom s))
  --, _directions :: !
  , _values :: !v
  }


newtype Game s a = Game
  { unGame :: KatipContextT (StateT (World s) IO) a
  } deriving newtype (Functor, Applicative, Monad, MonadIO, Katip, KatipContext, MonadState (World s))

-- | All 'KatipLogger's fulfill the logging interface described by 'Logger'.
instance Logger (Game s) where
  debug = logItemM (toLoc ?callStack) DebugS . LogStr
  info = logItemM (toLoc ?callStack) InfoS . LogStr
  warn = logItemM (toLoc ?callStack) WarningS . LogStr
  err = logItemM (toLoc ?callStack) ErrorS . LogStr
  withContext n = katipAddNamespace (Namespace [toStrict $ TLB.toLazyText n])


instance MonadReader (World s) (Game s) where
  ask = get
  local f g = do
    s <- get
    put (f s)
    r <- g
    put s
    return r

--in case we have both a read-only and a read-write constraint on the world.

type HasRoomsC s r = (HasRooms s r)
type MonadWorldRO s m = (MonadReader (World s) m, Logger m)
type MonadWorldNoLog s m = (MonadReader (World s) m, MonadState (World s) m)


class HasRooms s r | s -> r where
  wmL :: Lens' s (Store (AbstractRoom r))

newtype YaiflItem a = YaiflItem
  { toKatipItem :: Item a
  } deriving newtype (Functor)

instance A.ToJSON a => A.ToJSON (YaiflItem a) where
    toJSON (YaiflItem Item{..}) = A.object $
      [ "level" A..= _itemSeverity
      , "message" A..= B.toLazyText (unLogStr _itemMessage)
      , "timestamp" A..= formatAsLogTime _itemTime
      , "ns" A..= let f = T.intercalate "➤" (filter (/= T.empty) $ unNamespace _itemNamespace) in if T.empty == f then "" else "❬"<>f<>"❭"
      , "loc" A..= fmap reshapeFilename _itemLoc
      ] ++ ["data" A..=  _itemPayload | A.encode _itemPayload /= "{}"]

reshapeFilename :: Loc -> String
reshapeFilename Loc{..} = drop 1 (dropWhile (/= '/') loc_filename) <> ":" <> show (fst loc_start) <> ":" <> show (snd loc_start)

makeLenses ''World
makeLenses ''Object
makeLenses ''Args
makeLenses ''Rulebook
makeLenses ''ThingData
makeLenses ''RoomData
makeLenses ''Enclosing
makeLenses ''TimestampedObject
makeLenses ''Container
makePrisms ''ObjectSpecifics
makePrisms ''ThingWearability
makeLenses ''LocaleVariables
makeLenses ''LocaleInfo
makeLenses ''WorldModel

--instance HasBuffer (World s) 'LogBuffer where
--  bufferL _ = messageBuffers % _2

instance HasBuffer (World s) 'SayBuffer where
  bufferL _ = messageBuffers % _1

instance Functor (Args s) where
  fmap f = argsVariables %~ f
instance Functor (Object s) where
  fmap f = objData %~ f
instance Foldable (Object s) where
  foldMap f = f . _objData
instance Traversable (Object s) where
  traverse f o = (\v -> o {_objData = v}) <$> f (_objData o)


instance HasRooms (WorldModel s' d v o) s' where
  wmL = rooms
  type StoreLens' s d = (Lens' (World s) (Store (AbstractObject s d)))


type LocalePriorities s = Store (LocaleInfo s)

data LocaleInfo s = LocaleInfo
  { _priority :: Int
  , _localeObject :: AnyObject s
  , _isMentioned :: Bool
  }
-}