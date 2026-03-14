module ExampleUtils where

import Yaifl.Prelude
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Text.Read (readMaybe)
import qualified Data.List as L

import Data.Char (isSpace)
import Language.Haskell.TH
import Language.Haskell.TH.Quote hiding (quoteExp)
import Yaifl
import Yaifl.Rule
import Yaifl.Text.ResponseCollection

import qualified Data.Text as T

import Yaifl.Effects.Print
import Yaifl.Text.Verb

import Yaifl.ObjectSpecifics
import Yaifl.Effects.ObjectQuery
import Yaifl.Std.Rulebooks.ActionProcessing
import Yaifl.Effects.RuleEffects
import Yaifl.Effects.Interpreters

expQQ :: (String -> Q Exp) -> QuasiQuoter
expQQ quoteExp = QuasiQuoter quoteExp notSupported notSupported notSupported where
  notSupported _ = fail "Quotation in this context is not supported"

wrappedText :: QuasiQuoter
wrappedText = expQQ
  (return . LitE . StringL . toString . T.strip . newlinesToWrap . unindent . tabsToSpaces . toText)

lineIndent :: Text -> Int
lineIndent = T.length . T.takeWhile (== ' ')

minimumIndent :: Text -> Maybe Int
minimumIndent =
  listToMaybe . sort . map lineIndent
    . filter (\t -> T.empty /= T.dropWhile isSpace t) . lines

unindent :: Text -> Text
unindent s =
  case lines s of
    h : t ->
      let
        unindentedHead = T.dropWhile (== ' ') h
        minimumTailIndent = minimumIndent . unlines $ t
        unindentedTail = case minimumTailIndent of
          Just indent -> map (T.drop indent) t
          Nothing -> t
      in unlines $ unindentedHead : unindentedTail
    [] -> ""

tabsToSpaces :: Text -> Text
tabsToSpaces = T.replace "\t" " "

newlinesToWrap :: Text -> Text
newlinesToWrap = foldl' (\acc -> \case
  "" -> acc <> "\n" <> (if fmap snd (unsnoc acc) == Just '\n' then "" else "\n")
  x -> if T.empty == acc || T.last acc == '\n' then acc <> x else acc <> " " <> x) "" . lines


getToken :: IO T.Text
getToken = TIO.readFile "./auth-token.secret"

getGuildId :: IO GuildId
getGuildId = do
  gids <- readFile "./guildid.secret"
  case readMaybe gids of
    Just g -> pure g
    Nothing -> error "could not read guild id from `guildid.secret`"

-- | Given the test server and an action operating on a channel id, get the
-- first text channel of that server and use the action on that channel.
actionWithChannelId :: GuildId -> (ChannelId -> DiscordHandler a) -> DiscordHandler a
actionWithChannelId testserverid f = do
  Right chans <- restCall $ R.GetGuildChannels testserverid
  (f . channelId) (L.head (filter isTextChannel chans))
  where
    isTextChannel :: Channel -> Bool
    isTextChannel ChannelText {} = True
    isTextChannel _ = False
