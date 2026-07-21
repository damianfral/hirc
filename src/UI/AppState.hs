{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.AppState where

import Brick.Widgets.Edit
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime)
import IRC.Client
import IRC.Domain
import IRC.Protocol
import Relude

data ViewportName
  = Messages
  | Input
  | ChannelMembers
  | Channels
  deriving (Show, Ord, Eq)

data ChatMessage = ChatMessage
  { chatMessageTime :: UTCTime,
    -- | Nothing for nmessages comming from server
    chatMessageFrom :: Maybe Nickname,
    chatMessageContent :: [Text],
    chatMessageTag :: Tag
  }

data Tag = Normal | Dimmed | Mention

data ChannelState = ChannelState
  { channelMessages :: [ChatMessage],
    channelNicknames :: Set Nickname
  }

appendChannelMessages :: [ChatMessage] -> ChannelState -> ChannelState
appendChannelMessages msgs st = st {channelMessages = channelMessages st <> msgs}

modifyChannelNicknames ::
  (Set Nickname -> Set Nickname) -> ChannelState -> ChannelState
modifyChannelNicknames f st = st {channelNicknames = f (channelNicknames st)}

data AppState = AppState
  { appClient :: IRCClient,
    appUser :: User,
    appChannels :: Map Channel ChannelState,
    appCurrentChannel :: Maybe Channel,
    appHost :: Text,
    appHostMessages :: [ChatMessage],
    appInput :: Editor Text ViewportName
  }

updateState :: UTCTime -> Event -> AppState -> AppState
updateState ts (Connected (Server server) _welcome) = appendServerChatMessage chatMsg
  where
    chatMsg = ChatMessage ts Nothing ["Connected to " <> server] Dimmed
updateState ts (MessageReceived user (TargetChannel channel) msg) =
  appendChatMessage (ChatMessage ts (Just $ nickname user) (lines msg) Normal) channel
updateState ts (NoticeReceived user (TargetChannel channel) msg) =
  appendChatMessage chatMsg channel
  where
    chatMsg = ChatMessage ts (Just $ nickname user) (lines msg) Dimmed
updateState ts (UserJoined user channel) =
  appendChatMessage chatMsg channel
    . addNicknameToChannel (nickname user) channel
  where
    chatMsg = ChatMessage ts Nothing [nickOf user <> " joined"] Dimmed
updateState ts (UserLeft user channel reason) =
  appendChatMessage chatMsg channel
    . removeNicknameFromChannel (nickname user) channel
  where
    reasonText = case reason of Nothing -> ""; Just (Reason r) -> ", " <> r
    chatMsg =
      ChatMessage ts Nothing [nickOf user <> " left" <> reasonText] Dimmed
updateState ts (NickChanged user n@(Nickname nick)) =
  updateNick (nickname user) n . broadcastToAllChannels chatMsg
  where
    chatText = nickOf user <> " is now known as " <> nick
    chatMsg = ChatMessage ts Nothing [chatText] Dimmed
updateState _ts (ChannelUsers channel nicks) =
  modifyChannel channel $ modifyChannelNicknames (nicks <>)
updateState ts (UserDisconnected user _reason) =
  removeNicknameFromAllChannels (nickname user)
    . appendServerChatMessage chatMsg
  where
    chatText = nickOf user <> " disconnected"
    chatMsg = ChatMessage ts Nothing [chatText] Dimmed
updateState ts (ChannelListEntry channel count topic) =
  appendChatMessage chatMsg channel
  where
    countTxt = "(" <> show count <> " users)"
    chatTxts = [unwords ["[LIST] " <> channelToText channel, countTxt, topic]]
    chatMsg = ChatMessage ts Nothing chatTxts Dimmed
updateState ts (TopicReceived channel topic) = appendChatMessage chatMsg channel
  where
    chatMsg = ChatMessage ts Nothing ["[TOPIC] " <> topic] Dimmed
updateState ts (Disconnected reason) = \st ->
  let nick@(Nickname nickTxt) = nickname $ appUser st
      chatText = nickTxt <> " disconnected, " <> reason
      chatMsg = ChatMessage ts Nothing [chatText] Dimmed
   in removeNicknameFromAllChannels nick $ broadcastToAllChannels chatMsg st
updateState ts (MotdLine line) = appendServerChatMessage chatMsg
  where
    chatMsg = ChatMessage ts Nothing [line] Dimmed
updateState ts (ServerMessage text) = appendServerChatMessage chatMsg
  where
    chatMsg = ChatMessage ts Nothing [text] Dimmed
updateState _ _ = id

--------------------------------------------------------------------------------

modifyChannel ::
  Channel -> (ChannelState -> ChannelState) -> AppState -> AppState
modifyChannel channel update st =
  st {appChannels = Map.adjust update channel (appChannels st)}

modifyAllChannels :: (ChannelState -> ChannelState) -> AppState -> AppState
modifyAllChannels f st = st {appChannels = appChannels st <&> f}

appendChatMessage :: ChatMessage -> Channel -> AppState -> AppState
appendChatMessage msg channel =
  modifyChannel channel $ appendChannelMessages [msg]

appendServerChatMessage :: ChatMessage -> AppState -> AppState
appendServerChatMessage msg st =
  st {appHostMessages = appHostMessages st <> [msg]}

broadcastToAllChannels :: ChatMessage -> AppState -> AppState
broadcastToAllChannels chatMsg =
  modifyAllChannels $ appendChannelMessages [chatMsg]

addNicknameToChannel :: Nickname -> Channel -> AppState -> AppState
addNicknameToChannel user channel = modifyChannel channel $ \uiChannel ->
  uiChannel {channelNicknames = Set.insert user $ channelNicknames uiChannel}

removeNicknameFromChannel :: Nickname -> Channel -> AppState -> AppState
removeNicknameFromChannel nick channel =
  modifyChannel channel $ modifyChannelNicknames $ Set.delete nick

removeNicknameFromAllChannels :: Nickname -> AppState -> AppState
removeNicknameFromAllChannels nick =
  modifyAllChannels $ modifyChannelNicknames $ Set.delete nick

updateNick :: Nickname -> Nickname -> AppState -> AppState
updateNick old new =
  modifyAllChannels $ modifyChannelNicknames $ Set.delete old . Set.insert new

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

modifyUserInput ::
  (Editor Text ViewportName -> Editor Text ViewportName) -> AppState -> AppState
modifyUserInput f st = st {appInput = f $ appInput st}

resetUserInput :: AppState -> AppState
resetUserInput = modifyUserInput $ const emptyEditor

emptyEditor :: Editor Text ViewportName
emptyEditor = editorText Input Nothing ""

--------------------------------------------------------------------------------

nickOf :: User -> Text
nickOf = (\(Nickname n) -> n) . nickname
