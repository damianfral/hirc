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

data ChatMessage = ChatMessage
  { -- | Nothing for nmessages comming from server
    chatMessageFrom :: Maybe Nickname,
    chatMessageContent :: [Text],
    chatMessageTag :: Tag
  }

data Tag = Normal | Dimmed | Mention

data ChannelState = ChannelState
  { channelMessages :: [ChatMessage],
    channelNicknames :: Set Nickname
  }

data AppState = AppState
  { appClient :: IRCClient,
    appUser :: User,
    appChannels :: Map Channel ChannelState,
    appCurrentChannel :: Maybe Channel,
    appHost :: Text,
    appHostMessages :: [ChatMessage],
    appInput :: Editor Text ViewportName
  }

updateState :: Event -> AppState -> AppState
updateState (Connected server _welcome) = appendServerChatMessage chatMsg
  where
    chatMsg = ChatMessage Nothing ["Connected to " <> show server] Dimmed
updateState (MessageReceived user (TargetChannel channel) msg) =
  appendChatMessage (ChatMessage (Just $ nickname user) (lines msg) Normal) channel
updateState (NoticeReceived user (TargetChannel channel) msg) =
  appendChatMessage chatMsg channel
  where
    chatMsg = ChatMessage (Just $ nickname user) (lines msg) Dimmed
updateState (UserJoined user channel) =
  appendChatMessage chatMsg channel . addNicknameToChannel (nickname user) channel
  where
    chatMsg = ChatMessage Nothing [nickOf user <> " joined"] Dimmed
updateState (UserLeft user channel reason) =
  appendChatMessage chatMsg channel . addNicknameToChannel (nickname user) channel
  where
    reasonText = case reason of Nothing -> ""; Just (Reason r) -> ", " <> r
    chatMsg =
      ChatMessage Nothing [nickOf user <> "left ", reasonText] Dimmed
updateState (NickChanged user n@(Nickname nick)) = \st ->
  let channels = appChannels st & Map.keysSet
   in Set.fold (appendChatMessage chatMsg) st channels
        & modifyUserNick (nickname user) n
  where
    chatText = nickOf user <> " is now known as " <> nick
    chatMsg = ChatMessage Nothing [chatText] Dimmed
updateState (UserDisconnected user _reason) = \st ->
  let channels = appChannels st & Map.keysSet
   in Set.fold (appendChatMessage chatMsg) st channels
        & removeNicknameFromAllChannels (nickname user)
  where
    chatText = nickOf user <> " disconnected"
    chatMsg = ChatMessage Nothing [chatText] Dimmed
updateState (ChannelUsers channel nicks) = \st ->
  Set.fold (`addNicknameToChannel` channel) st nicks
updateState (ChannelListEntry channel count topic) =
  appendChatMessage chatMsg channel
  where
    chatTxts = ["[LIST] " <> channelToText channel <> "(" <> show count, " users)", topic]
    chatMsg = ChatMessage Nothing chatTxts Dimmed
updateState (TopicReceived channel topic) =
  appendChatMessage chatMsg channel
  where
    chatMsg = ChatMessage Nothing ["[TOPIC] " <> topic] Dimmed
updateState (Disconnected reason) = \st ->
  let channels = appChannels st & Map.keysSet
      nick@(Nickname nickTxt) = nickname $ appUser st
      chatText = nickTxt <> " disconnected, " <> reason
      chatMsg = ChatMessage Nothing [chatText] Dimmed
   in Set.fold (appendChatMessage chatMsg) st channels
        & removeNicknameFromAllChannels nick
updateState _ = id

--------------------------------------------------------------------------------

modifyChannel ::
  Channel -> (ChannelState -> ChannelState) -> AppState -> AppState
modifyChannel channel update st =
  st {appChannels = Map.adjust update channel (appChannels st)}

appendChatMessage :: ChatMessage -> Channel -> AppState -> AppState
appendChatMessage msg channel = modifyChannel channel $ \uiChannel ->
  uiChannel {channelMessages = channelMessages uiChannel <> [msg]}

appendServerChatMessage :: ChatMessage -> AppState -> AppState
appendServerChatMessage msg st = st {appHostMessages = appHostMessages st <> [msg]}

addNicknameToChannel :: Nickname -> Channel -> AppState -> AppState
addNicknameToChannel user channel = modifyChannel channel $ \uiChannel ->
  uiChannel {channelNicknames = Set.insert user $ channelNicknames uiChannel}

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
