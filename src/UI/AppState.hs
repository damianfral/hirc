{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.AppState where

import Brick.Widgets.Edit
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import IRC.Client
import IRC.Domain
import IRC.Protocol
import Relude
import UI.Chat

data AppState = AppState
  { appClient :: IRCClient,
    appUser :: User,
    appChats :: Map ChatID Chat,
    appCurrentChat :: ChatID,
    appServer :: Server,
    appInput :: Editor Text ViewportName
  }

data ViewportName = Messages | Input | ChannelMembers | Channels
  deriving (Show, Ord, Eq)

updateState :: UTCTime -> Event -> AppState -> AppState
updateState ts (Connected (Server server) _welcome) = \st ->
  let chatMsg = ChatMessage ts Nothing ["Connected to " <> server] ServerEvent
      chatID = ChatWithServer $ appServer st
   in st & updateChat chatID (appendChatMessage chatMsg)
updateState ts (MessageReceived user (TargetChannel channel) msg) =
  let chatMsg = ChatMessage ts (Just $ nickname user) (lines msg) Normal
   in updateChat (ChatWithChannel channel) $ appendChatMessage chatMsg
updateState ts (MessageReceived user (TargetUser _) msg) =
  let nick = nickname user
      chatMsg = ChatMessage ts (Just nick) (lines msg) Normal
   in updateChat (ChatWithNickname nick) $ appendChatMessage chatMsg
updateState ts (NoticeReceived user (TargetChannel channel) msg) =
  let chatMsg = ChatMessage ts (Just $ nickname user) (lines msg) Notice
   in updateChat (ChatWithChannel channel) $ appendChatMessage chatMsg
updateState ts (NoticeReceived user (TargetUser _) msg) =
  let routeToChannel = do
        afterHash <- T.stripPrefix "[#" msg
        let (chanName, _) = T.break (== ']') afterHash
        guard $ not (T.null chanName)
        Just chanName
   in case routeToChannel of
        Just chanName ->
          let chatMsg = ChatMessage ts (Just $ nickname user) (lines msg) Notice
           in updateChat (ChatWithChannel (Channel chanName)) $ appendChatMessage chatMsg
        Nothing ->
          let nick = nickname user
              chatMsg = ChatMessage ts (Just nick) (lines msg) Notice
           in updateChat (ChatWithNickname nick) $ appendChatMessage chatMsg
updateState ts (UserJoined user channel) =
  let chatMsg = ChatMessage ts Nothing [nickOf user <> " joined"] ServerEvent
   in updateChat
        (ChatWithChannel channel)
        (appendChatMessage chatMsg . addNickname (nickname user))
updateState ts (UserLeft user channel reason) =
  let reasonText = case reason of Nothing -> ""; Just (Reason r) -> ", " <> r
      chatText = nickOf user <> " left," <> reasonText
      chatMsg = ChatMessage ts Nothing [chatText] ServerEvent
   in updateChat
        (ChatWithChannel channel)
        (appendChatMessage chatMsg . addNickname (nickname user))
updateState ts (NickChanged user newNick@(Nickname n)) = \st ->
  let oldNick = nickname user
      hasUser = (oldNick `Set.member`) . chatMembers
      chatIDs = Map.keysSet $ Map.filter hasUser $ appChats st
      chatText = nickOf user <> " is now known as " <> n
      chatMsg = ChatMessage ts Nothing [chatText] ServerEvent
      chatUpdate =
        removeNickname oldNick . addNickname newNick . appendChatMessage chatMsg
      updates = toList chatIDs <&> (`updateChat` chatUpdate)
   in foldl' (&) st updates
updateState _ts (ChannelUsers channel nicks) =
  updateChat (ChatWithChannel channel) $ addNicknames $ toList nicks
updateState ts (UserDisconnected user _reason) = \st ->
  let hasUser = (nickname user `Set.member`) . chatMembers
      chatIDs = Map.keysSet $ Map.filter hasUser $ appChats st
      chatText = nickOf user <> " disconnected"
      chatMsg = ChatMessage ts Nothing [chatText] ServerEvent
      chatUpdate = removeNickname (nickname user) . appendChatMessage chatMsg
      updates = toList chatIDs <&> (`updateChat` chatUpdate)
   in foldl' (&) st updates
updateState ts (ChannelListEntry channel count topic) = \st ->
  let countTxt = "(" <> show count <> " users)"
      chatTxts = [unwords [channelToText channel, countTxt, topic]]
      chatMsg = ChatMessage ts Nothing chatTxts CommandReply
   in st & updateChat (appCurrentChat st) (appendChatMessage chatMsg)
updateState ts (TopicReceived channel topic) =
  let chatMsg = ChatMessage ts Nothing [topic] Info
   in updateChat (ChatWithChannel channel) $ appendChatMessage chatMsg
updateState ts (Disconnected reason) = \st ->
  let nick@(Nickname nickTxt) = nickname $ appUser st
      chatText = nickTxt <> " disconnected, " <> reason
      chatMsg = ChatMessage ts Nothing [chatText] ServerEvent
   in st & updateAllChats (appendChatMessage chatMsg . removeNickname nick)
updateState ts (MotdLine line) = \st ->
  let chatMsg = ChatMessage ts Nothing [line] ServerEvent
   in st & updateChat (ChatWithServer $ appServer st) (appendChatMessage chatMsg)
updateState ts (ServerMessage text) = \st ->
  let chatMsg = ChatMessage ts Nothing [text] Normal
   in st & updateChat (ChatWithServer $ appServer st) (appendChatMessage chatMsg)

--------------------------------------------------------------------------------

updateChat :: ChatID -> (Chat -> Chat) -> AppState -> AppState
updateChat cid update st =
  let go = Just . update . fromMaybe (Chat mempty mempty 0)
   in st {appChats = Map.alter go cid (appChats st)}

updateAllChats :: (Chat -> Chat) -> AppState -> AppState
updateAllChats f st = st {appChats = appChats st <&> f}

goToNextChat :: AppState -> AppState
goToNextChat st =
  let chatIDs = Map.keysSet (appChats st)
      nextChat = Set.lookupGT (appCurrentChat st) chatIDs
      chatID = fromMaybe (ChatWithServer $ appServer st) $ do
        nextChat <|> Set.lookupMin chatIDs
   in st {appCurrentChat = chatID}

goToPrevChat :: AppState -> AppState
goToPrevChat st =
  let chatIDs = Map.keysSet (appChats st)
      prevChat = Set.lookupLT (appCurrentChat st) chatIDs
      chatID = fromMaybe (ChatWithServer $ appServer st) $ do
        prevChat <|> Set.lookupMax chatIDs
   in st {appCurrentChat = chatID}

markChatRead :: ChatID -> AppState -> AppState
markChatRead cID = updateChat cID readChat

removeChat :: ChatID -> AppState -> AppState
removeChat cid st =
  let newChats = Map.delete cid $ appChats st
      st' = st {appChats = newChats}
   in if appCurrentChat st == cid then goToNextChat st' else st'

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
