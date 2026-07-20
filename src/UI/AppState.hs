{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.AppState where

import Brick
import Brick.Widgets.Edit
import qualified Data.Map as Map
import qualified Data.Set as Set
import IRC.Client
import IRC.Domain
import IRC.Protocol
import Relude

data ViewportName
  = Messages
  | Input
  | ChannelMembers
  | Channels
  | Scrollable ClickableScrollbarElement ViewportName
  deriving (Show, Ord, Eq)

data ChannelState = ChannelState
  { channelMessages :: [Text],
    channelNicknames :: Set Nickname
  }

data AppState = AppState
  { appClient :: IRCClient,
    appUser :: User,
    appChannels :: Map Channel ChannelState,
    appCurrentChannel :: Maybe Channel,
    appHost :: Text,
    appHostMessages :: [Text],
    appInput :: Editor Text ViewportName
  }

updateState :: Event -> AppState -> AppState
updateState (Connected server _welcome) =
  appendServerMessage $ "Connected to " <> show server
updateState (MessageReceived user (TargetChannel channel) msg) =
  appendMessage msg channel (Just $ nickname user)
updateState (NoticeReceived user (TargetChannel channel) msg) =
  appendMessage ("[NOTICE] " <> nickOf user <> ": " <> msg) channel Nothing
updateState (UserJoined user channel) =
  let msg = "--> " <> nickOf user <> " joined"
   in modifyChannel channel $ \uiChannel@ChannelState {..} ->
        uiChannel
          { channelMessages = channelMessages <> [msg],
            channelNicknames = Set.insert (nickname user) channelNicknames
          }
updateState (UserLeft user channel _reason) =
  appendMessage ("<-- " <> nickOf user <> " left") channel Nothing
    . removeNicknameFromChannel (nickname user) channel
updateState (NickChanged user n@(Nickname nick)) =
  appendServerMessage (nickOf user <> " is now known as " <> nick)
    . modifyUserNick (nickname user) n
updateState (UserDisconnected user _reason) =
  appendServerMessage ("<-- " <> nickOf user <> " quit")
    . removeNicknameFromAllChannels (nickname user)
updateState (ChannelUsers channel nicks) = addNicknamesToChannel nicks channel
updateState (ChannelListEntry channel count topic) =
  let msg =
        unwords
          ["[LIST]", channelToText channel, "(" <> show count, " users)", topic]
   in appendMessage msg channel Nothing
updateState (TopicReceived channel topic) =
  appendMessage ("Topic: " <> topic) channel Nothing
updateState (Disconnected reason) =
  appendServerMessage $ "Disconnected: " <> reason
updateState _ = id

--------------------------------------------------------------------------------

modifyChannel ::
  Channel -> (ChannelState -> ChannelState) -> AppState -> AppState
modifyChannel channel update st =
  st {appChannels = Map.adjust update channel (appChannels st)}

appendMessage :: Text -> Channel -> Maybe Nickname -> AppState -> AppState
appendMessage msg channel Nothing =
  modifyChannel channel $ \uiChannel ->
    uiChannel {channelMessages = channelMessages uiChannel <> [msg]}
appendMessage msg channel (Just (Nickname nick)) =
  appendMessage (nick <> ": " <> msg) channel Nothing

appendServerMessage :: Text -> AppState -> AppState
appendServerMessage msg st = st {appHostMessages = appHostMessages st <> [msg]}

addNicknamesToChannel :: Set Nickname -> Channel -> AppState -> AppState
addNicknamesToChannel users channel = modifyChannel channel $ \uiChannel ->
  uiChannel {channelNicknames = users <> channelNicknames uiChannel}

removeNicknameFromChannel :: Nickname -> Channel -> AppState -> AppState
removeNicknameFromChannel user channel =
  modifyChannel channel $ \uiChannel@ChannelState {..} ->
    uiChannel {channelNicknames = Set.delete user channelNicknames}

removeNicknameFromAllChannels :: Nickname -> AppState -> AppState
removeNicknameFromAllChannels nick = modifyAllChannels $ \uiChannel ->
  uiChannel {channelNicknames = Set.delete nick $ channelNicknames uiChannel}

modifyAllChannels :: (ChannelState -> ChannelState) -> AppState -> AppState
modifyAllChannels update st = st {appChannels = update <$> appChannels st}

modifyUserNick :: Nickname -> Nickname -> AppState -> AppState
modifyUserNick old new = modifyAllChannels $ \uiChannel ->
  let newNicks = Set.delete old $ Set.insert new $ channelNicknames uiChannel
   in uiChannel {channelNicknames = newNicks}

goToNextChannel :: AppState -> AppState
goToNextChannel st = fromMaybe st $ do
  channel <- appCurrentChannel st
  Just $ st {appCurrentChannel = Just $ nextChannel channel (appChannels st)}

goToPrevChannel :: AppState -> AppState
goToPrevChannel st = fromMaybe st $ do
  channel <- appCurrentChannel st
  Just $ st {appCurrentChannel = Just $ prevChannel channel (appChannels st)}

nextChannel :: Channel -> Map Channel ChannelState -> Channel
nextChannel currentChannel channels =
  Map.keysSet channels & \set ->
    fromMaybe currentChannel $ do
      i <- Set.lookupIndex currentChannel set
      listToMaybe $ drop (i + 1) $ toList set

prevChannel :: Channel -> Map Channel ChannelState -> Channel
prevChannel currentChannel channels =
  Map.keysSet channels & \set ->
    fromMaybe currentChannel $ do
      i <- Set.lookupIndex currentChannel set
      listToMaybe $ drop (i - 1) $ toList set

--------------------------------------------------------------------------------

nickOf :: User -> Text
nickOf = (\(Nickname n) -> n) . nickname
