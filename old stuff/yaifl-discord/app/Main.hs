
import Yaifl.Prelude
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Chan
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types

import ExampleUtils
import Yaifl.Std.Rulebooks.ActionProcessing
import Yaifl
import Breadcrumbs
import Yaifl.Std.Parser
import Yaifl.Std.Kinds.Direction
import Yaifl.Text.Print
import Yaifl.Std.Actions.Collection
import Effectful.Error.Static (runError)

import Yaifl.Std.Actions.Looking.Visibility
import Effectful.Provider.List (type (++))
import Yaifl.Std.EffectHandlers
import Yaifl.Core.Effects
import Yaifl.Std.Actions.Imports
import Yaifl.Std.Create
import Yaifl.Std.Actions.Going
import Yaifl.Core.ObjectLike
import Yaifl.Core.Rules.Run
import Yaifl.Text.ResponseCollection
import Yaifl.Core.Rules.RuleEffects
import Discord.Internal.Rest
import qualified Data.List as L
import qualified Discord.Requests as R
import qualified Data.Text as T

import qualified Prettyprinter as PP
import Effectful.TH ( makeEffect )
import Effectful.Dispatch.Dynamic (interpret)
import qualified Data.Text as T
import Yaifl.Core.Query.Region
import Data.Maybe (fromJust)
import Yaifl.Object.Kind
import Yaifl.Tag
import Yaifl.Std.Kinds.Supporter
import Yaifl.Std.Kinds.Container

portRoyal3World :: Game PlainWorldModel ()
portRoyal3World = do
  setTitle "1691"
  fj <- addRoom "Fort James"
    ! #description [wrappedText|The enclosure of Fort James is a large, roughly hexagonal court walled with heavy stone.
The walls face the entrance to Port Royal Harbour, to the south, and the battery of guns is prepared to destroy any enemy ship arriving.|]
    ! done

  ts <- addRoom "Thames Street End"
    ! #description [wrappedText|The ill-named Thames Street runs from here -- at the point of the peninsula -- all the way east among houses and shops,
through the Fish Market, edging by the round front of Fort Carlisle, to the point where the town stops and there is only sandy spit beyond. Most of that stretch is
full of people at all hours. Imported goods are moved off of ships and taken to distributors; exported goods are brought to be loaded; and there is one public house and brothel for every ten inhabitants.

Lime Street, wider and healthier but not as rich, runs directly south, and to the north the road opens up into the courtyard of Fort James.|]
    ! done
  ts `isSouthOf` fj

  ls <- addRoom "Lime Street" ! done
  ls `isSouthOf` ts
  fr <- addRoom "Fisher's Row"
    ! #description [wrappedText|"A waterfront street that runs south towards Chocolata Hole, where the small craft are harboured.
It also continues north around the tip of the peninsula from here, turning into the east-west Thames Street.|]
    ! done

  ts `isBelow` fj
  ts `isNowhere` Up
  fr `isWestOfOneWay` ts
  ts `isNorthOfOneWay` fr

  wl <- addRoom "Water Lane"
    ! #description [wrappedText|Here Thames Street -- never very straight -- goes steeply southeast for a portion
before continuing more directly to the east.

Water Lane runs south toward Queen Street, and facing onto it is the New Prison -- which, in the way of these things,
is neither. It did serve in that capacity for a time, and in a measure of the villainy which has been usual in
Port Royal from its earliest days, it is nearly the largest building in the town.|]
    ! done
  wl `isEastOf` ts

  tsawb <- addRoom' "Thames Street at Wherry Bridge" ! #description
    "To the southwest is the fishmarket; directly across the street is the entrance to a private alley through a brick archway."
  tsawb `isEastOf` wl

  tfm <- addRoom "The Fishmarket" ! done
  tfm `isSouthWestOf` tsawb

  tpa <- addRoom' "The Private Alley" ! #description [wrappedText|You're just outside the tavern the Feathers. To the north, under a
pretty little archway, is the active mayhem of Thames Street, but the alley narrows down to a dead end a
little distance to the south.|]
  tpa `isSouthOf` tsawb

  tf <- addRoom' "The Feathers" ! #description [wrappedText|Newly built with brick, replacing the older Feathers tavern that used to stand here.
It sells wines in quantity, as well as serving them directly, and the goods are always of the best quality.
There's a room upstairs for those wanting to stay the night.|]
  tf `isInsideFrom` tpa

  tsbtkh <- addRoom' "Thames Street by the King's House" ! #description [wrappedText|The King's House is reserved for the use of the Governor, but he does not live in it,
and it is frequently being rented out to some merchant so that the government will at least derive some value from it. It is nearly the least interesting
establishment on Thames Street, and the crowd -- which, to the west, is extremely dense -- here thins out a bit.|]
  tsbtkh `isEastOf` tsawb

  tsbfc <- addRoom' "Thames Street before Fort Carlisle" ! #description [wrappedText|Here Thames Street, formerly a respectable width, narrows to a footpath in order to
edge around the front of Fort Carlisle, underneath the mouths of the cannon.

There are no buildings on the harbour side of Thames Street at this point, which means that you have an unusually good view of the ships at dock,
water beyond, and the Blue Mountains rising on the other side of the harbour.|]
  tsbfc `isEastOf` tsbtkh
  fc <- addRoom' "Fort Carlisle" ! #description [wrappedText|Handsomely arrayed with cannons which you could fire at any moment -- though of course
there are ships at dock which might be in the way.|]
  fc `isSouthOf` tsbfc
  tfb <- addRoom' "The Feathers Bedroom" ! done
  tfb `isAbove` tf

  qsm <- addRoom' "Queen Street Middle" ! done
  qsen <- addRoom' "Queen Street End" ! done
  qse <- addRoom' "Queen Street East" ! done
  qsatp <- addRoom' "Queen Street at the Prison" ! done
  qse `isSouthOf` ls
  qse `isEastOf` qsm
  qse `isSouthOf` tpa
  qsm `isEastOf` qsen
  qsatp `isEastOf` qse

  i <- addRegion "Inland"
  [qsen, qsm, qse, tpa, ls, qsatp] `areInRegion` i
  wf <- addRegion "Waterfront"
  [tsbtkh, tsbfc, tsawb, wl, tfm, fr, ts] `areInRegion` wf
  mh <- addRegion "Military Holdings"
  [fc, fj] `areInRegion` mh
  t <- addRegion "Tavern"
  t `isSubregionOf` i
  [tf, tfb] `areInRegion` t

escapeWorld :: Game PlainWorldModel ()
escapeWorld = do
  setTitle "Escape"
  yb <- addRoom "Your Bedroom" ! done
  bt <- addSupporter "billiards table" ! done
  tc <- addContainer "trophy cup"
    ! #location (onThe bt)
    ! done
  addThing "starting pistol"
    ! #location (inThe tc)
    ! done
  th <- addRoom "Treehouse" ! done

  addContainer "cardboard box" ! done
  pass
  gs <- addRoom "Grassy Slope" ! #modify makeNameImproper ! done
  th `isAbove` gs
  w <- addDoor "bedroom window"
    ! #front (yb, West)
    ! #back (gs, East)
    ! done
  insteadOf #searching [theObject w] $ \_ -> do
    bs <- getOtherSideOfDoor w
    [saying|Through the window, you make out {the bs}.|]
  insteadOf #climbing [theObject w] $ tryAction "enter" [TheThing $ coerceTag w]

  -- the original requires you to define "climb through [something]" as an alias, whereas
  -- my parser will just assume you want to climb something called the "through window" and considers
  -- one word enough of a match.

  -- I don't know if this needs fixing but if I leave this here for when I inevitably rewrite the parser it'll help.
  insteadOf #going [throughTheClosedDoor w] $ const [saying|The window is shut: you'd break the glass.|]
  pass

convertStack ::
  forall wm es' a.
  IOE :> es'
  => (Ord (WMDirection wm), Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
  => HasLookingProperties wm
  => World wm
  -> ActionCollection wm
  -> ChannelId
  -> RestChanHandle
  -> GuildId
  -> Eff (EffStack wm ++ es') a
  -> Eff es' (a, World wm)
convertStack w ac cid restChan g =
  fmap (either (error . show) id)
  . inject
  . runError
  . runBreadcrumbs Nothing
  . runStateShared w
  . runPrintPure
  . zoomState #actions
  . zoomState @(World wm) #metadata
  . runQueryAsLookup
  . runTraverseAsLookup
  . evalStateShared ac
  . runInputAsBuffer
  . zoomState @(World wm) #activities
  . zoomState @(World wm) #responses
  . zoomState @(World wm) #adaptiveNarrative
  . runActionHandlerAsWorldActions

beginPlay ::
  YaiflEffects PlainWorldModel es
  => WorldActions PlainWorldModel
  -> Eff es ()
beginPlay wa = do
  failHorriblyIfMissing (runRulebook Nothing False (wa ^. #whenPlayBegins) ())
  pass

main :: IO ()
main = gatewayExample

data ConstructionOptions wm = ConstructionOptions
  { activityCollectionBuilder :: ActivityCollection wm -> ActivityCollector wm
  , responseCollectionBuilder :: ResponseCollection wm -> ResponseCollector wm
  }

defaultOptions :: (WMActivities wm ~ ActivityCollection wm, WMResponses wm ~ ResponseCollection wm) => ConstructionOptions wm
defaultOptions = ConstructionOptions ActivityCollector ResponseCollector


makeWorld ::
  HasStandardProperties wm
  => WMHasObjSpecifics wm
  => ConstructionOptions wm
  -> Text
  -> Game wm b
  -> IO (World wm)
makeWorld conOptions fullTitle initWorld = do
  let emptyWorld = blankWorld (activityCollectionBuilder conOptions) (responseCollectionBuilder conOptions)
  snd <$$> runGame runPrintPure runInputAsBuffer emptyWorld blankActionCollection $ do
    withSpan' "worldbuilding" fullTitle $ do
      newWorld
      initWorld
      -- this just moves the actions from the indexed, static, standard library collection
      -- into the dynamic collection
      -- we do it here because we need to copy over changes to actions and we can't modify WrappedActions directly
      addStandardActions

runPrintAsDiscordHandler ::
  forall s es a.
  IOE :> es
  => PartialState s MessageBuffer es
  => ChannelId
  -> RestChanHandle
  -> GuildId
  -> Eff (Print : es) a
  -> Eff es a
runPrintAsDiscordHandler chan restChan testserverid = interpret $ \_ -> \case
  PrintDoc mbMetadata doc -> do
    r <- processDoc doc
    modify (\s -> s & buf % (#buffer @(Lens' (MessageBuffer) [StyledDoc MessageAnnotation])) %~ (r:))
    whenJust mbMetadata $ \metadata -> modify (\s -> s & buf % #lastMessageContext @(Lens' (MessageBuffer) MessageContext) .~ metadata)
    liftIO $ writeRestCall restChan (R.CreateMessage chan (show r))
    pass
  SetStyle mbStyle -> setStyle' mbStyle
  ModifyBuffer f -> do
    modify (\s -> s & buf %~ f)
    use buf
  GetBuffer -> use buf

setStyle' ::
  forall s es.
  PartialState s (MessageBuffer) es
  => Maybe MessageAnnotation -- ^ The updated style.
  -> Eff es ()
setStyle' s = buf % (#style @(Lens' (MessageBuffer) (Maybe MessageAnnotation))) .= s

-- | Prints every event as it happens
gatewayExample :: IO ()
gatewayExample = do
  tok <- getToken
  testserverid <- getGuildId

  outChan <- newChan :: IO (Chan String)
  gameChan <- newChan :: IO (Chan Text)
  printQueue <- newChan :: IO (Chan Text)
  (restChan, restThreadId) <- startRestThread (Auth tok) printQueue
  Right cs <- writeRestCall restChan (R.GetGuildChannels testserverid)
  let cid = channelId (L.head $ filter (\x -> channelName x == "botspam" || channelName x == "general" || channelName x == "yaifl-bot") cs)
  -- Events are processed in new threads, but stdout isn't
  -- synchronized. We get ugly output when multiple threads
  -- write to stdout at the same time
  threadId <- forkIO $ forever $ readChan outChan >>= putStrLn
  gameThread <- forkIO $ do
    w <- makeWorld defaultOptions "AWA" escapeWorld
    r <- runEff $ convertStack w blankActionCollection cid restChan testserverid $ do
          withSpan' "run" "awa" $ do
            setPostPromptSpacing False
            wa <- get @(WorldActions PlainWorldModel)
            beginPlay wa
            --when I write a proper game loop, this is where it needs to go
            runTurnsFromDiscord restChan gameChan cid
    let msgList = (view $ _2 % #messageBuffer % #buffer % reversed) r
    writeChan outChan $ (mconcat . map show) msgList
    threadDelay $ 2 * 10 ^ 6
    res <- writeRestCall restChan (R.CreateMessage (channelId (L.head $ filter isTextChannel cs)) ((mconcat . map show) msgList))
    putStrLn $ show res

  err <- runDiscord $ def { discordToken = tok
                          , discordOnStart = startHandler testserverid
                          , discordOnEvent = eventHandler gameChan outChan
                          , discordOnEnd = killThread threadId
                          }
  TIO.putStrLn err

runTurnsFromDiscord ::
  forall wm es.
  IOE :> es
  => RuleEffects wm es
  => SayableValue (WMText wm) wm
  => State (World wm) :> es
  => State (WorldActions wm) :> es
  => RestChanHandle
  -> Chan Text
  -> ChannelId
  -> Eff es ()
runTurnsFromDiscord restChan gameChan chanId = do
  w <- get @(World wm)
  let msgList = (view $ #messageBuffer % #buffer % reversed) w
  unless (null msgList) $ do
    liftIO $ writeRestCall restChan (R.CreateMessage chanId ((mconcat . map show) msgList))
    modifyBuffer (#buffer .~ [])
    pass
  actualInput <- liftIO $ readChan gameChan
  let actionOpts = ActionOptions False False
  wa <- get @(WorldActions wm)
  -- runRulebook Nothing False (wa ^. #turnSequence) ()
  printPrompt actionOpts
  withStyle (Just bold) $ printText actualInput
  void $ parseAction actionOpts [NoParameter] actualInput
  void $ runRulebook Nothing False (wa ^. #turnSequence) ()
  runTurnsFromDiscord restChan gameChan chanId

-- Events are enumerated in the discord docs
-- https://discord.com/developers/docs/topics/gateway#commands-and-events-gateway-events
eventHandler :: Chan Text -> Chan String -> Event -> DiscordHandler ()
eventHandler gameChan out event = case event of
  MessageCreate m -> when (not (fromBot m) && isPing m) $ do
    void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
    liftIO $ writeChan gameChan (T.drop 1 . messageContent $ m)
    liftIO $ threadDelay (2 * 10 ^ (6 :: Int))
  _ -> pass


fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = (">" `T.isPrefixOf`) . T.toLower . messageContent

startHandler :: GuildId -> DiscordHandler ()
startHandler testserverid = do
  let opts = RequestGuildMembersOpts
        { requestGuildMembersOptsGuildId = testserverid
        , requestGuildMembersOptsLimit = 100
        , requestGuildMembersOptsNamesStartingWith = ""
        }

  -- gateway commands are enumerated in the discord docs
  -- https://discord.com/developers/docs/topics/gateway#commands-and-events-gateway-commands
  sendCommand (RequestGuildMembers opts)

isTextChannel :: Channel -> Bool
isTextChannel ChannelText {} = True
isTextChannel _ = False

processDoc ::
  forall s es.
  PartialState s (MessageBuffer) es
  => StyledDoc MessageAnnotation
  -> Eff es (StyledDoc MessageAnnotation)
processDoc msg = do
  (MessageBuffer _ _ style cxt _) <- use @s buf
  -- if we have no context, we just monoid it.
  let joinOp = case cxt of
        [] -> (<>)
        _ -> (PP.<+>)
  return $ PP.hcat cxt `joinOp` maybe id PP.annotate style msg