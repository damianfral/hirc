{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.EventHandler where

import Brick
import qualified Brick.Keybindings.KeyDispatcher as KD
import Brick.Keybindings.Pretty (keybindingTextTable)
import Brick.Widgets.Edit (applyEdit, getEditContents, handleEditorEvent)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.Zipper (breakLine)
import Data.Time.Clock (getCurrentTime)
import qualified Graphics.Vty as V
import IRC.Client
import IRC.Domain
import IRC.Protocol (Nickname (..), User (..))
import Relude
import UI.AppState
import UI.Chat
import UI.KeyEvent

handleEvent :: BrickEvent ViewportName Event -> EventM ViewportName AppState ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'o') [V.MCtrl])) =
  modify $ modifyUserInput $ applyEdit breakLine
handleEvent e@(VtyEvent (V.EvKey key mods)) = do
  handled <- dispatcher
  when (not handled) $ do
    st <- get
    newEditor <- nestEventM' (appInput st) $ handleEditorEvent e
    modify $ modifyUserInput $ const newEditor
  where
    dispatcher = case keyDispatcher of
      Nothing -> pure False
      Just kd -> KD.handleKey kd key mods
handleEvent (VtyEvent _) = pure ()
handleEvent (AppEvent event) = do
  ts <- liftIO getCurrentTime
  modify $ updateState ts event
  scrollMessagesToEnd
handleEvent (MouseDown vp direction _mods _location) = case direction of
  V.BScrollUp -> handleScroll vp Brick.Up
  V.BScrollDown -> handleScroll vp Brick.Down
  _ -> pure ()
handleEvent (MouseUp {}) = pure ()

scrollMessagesToEnd :: EventM ViewportName s ()
scrollMessagesToEnd = vScrollToEnd $ viewportScroll Messages

handleScroll :: ViewportName -> Direction -> EventM ViewportName s ()
handleScroll = vScrollPage . viewportScroll

-- TODO: we are ignoring conflicting keybindings
keyDispatcher :: Maybe (KD.KeyDispatcher KeyEvent (EventM ViewportName AppState))
keyDispatcher = case KD.keyDispatcher keyConfig keyEventHandlers of
  Right d -> Just d
  Left _conflictingKeybindings -> Nothing

keyEventHandlers :: [KD.KeyEventHandler KeyEvent (EventM ViewportName AppState)]
keyEventHandlers =
  [ KD.onEvent EvQuit "Quit the application" haltWithQuit,
    KD.onEvent EvScrollUp "Scroll messages up" $ handleScroll Messages Brick.Up,
    KD.onEvent EvScrollDown "Scroll messages down" $ do
      handleScroll Messages Brick.Down,
    KD.onEvent EvNextChannel "Go to next channel" $ do
      modify goToNextChat >> scrollMessagesToEnd,
    KD.onEvent EvPrevChannel "Go to previous channel" $ do
      modify goToPrevChat >> scrollMessagesToEnd,
    KD.onEvent EvActivate "Send message or run command" handleEnter
  ]

haltWithQuit :: EventM ViewportName AppState ()
haltWithQuit = do
  AppState {..} <- get
  liftIO $ writeAction appClient (Quit Nothing)
  halt

handleEnter :: EventM ViewportName AppState ()
handleEnter = do
  AppState {..} <- get
  let content = T.intercalate "\n" $ T.strip <$> getEditContents appInput
  if "/" `T.isPrefixOf` content
    then handleCommand content
    else handleSendMessage content
  modify resetUserInput

handleCommand :: Text -> EventM ViewportName AppState ()
handleCommand cmd = case T.words cmd of
  ["/help"] -> handleHelp
  "/quit" : args | length args <= 1 -> handleQuit $ Reason <$> maybeAt 0 args
  ["/join", channel] -> handleJoin $ makeChannel channel
  ["/part"] -> handleLeave Nothing Nothing
  "/part" : args
    | length args == 1 -> handleLeave Nothing (Reason <$> maybeAt 0 args)
  "/part" : args
    | length args == 2 ->
        handleLeave (makeChannel <$> maybeAt 0 args) (Reason <$> maybeAt 1 args)
  ["/names"] -> handleNames
  ["/list"] -> handleList
  ["/nick", nickname] -> handleNick $ Nickname nickname
  "/away" : args | length args <= 1 -> handleAway $ Reason <$> maybeAt 0 args
  "/msg" : nick : msgWords -> handlePrivateMessage nick $ T.unwords msgWords
  ["/query", nick] -> handleQuery $ Nickname nick
  ["/closequery"] -> handleCloseQuery
  "/notice" : msgWords -> handleNotice $ T.unwords msgWords
  "/topic" : args | length args <= 1 -> handleTopic (maybeAt 0 args)
  _ -> handleHelp
  where
    makeChannel ch = Channel (T.dropWhile (== '#') ch)

handleJoin :: Channel -> EventM ViewportName AppState ()
handleJoin channel = do
  st <- get
  when (Map.notMember (ChatWithChannel channel) (appChats st)) $ do
    liftIO $ writeAction (appClient st) $ JoinChannel channel
  let chat = Chat mempty mempty 0
  let newChats = Map.insert (ChatWithChannel channel) chat (appChats st)
  modify $ \st' ->
    st' {appChats = newChats, appCurrentChat = ChatWithChannel channel}

handleLeave :: Maybe Channel -> Maybe Reason -> EventM ViewportName AppState ()
handleLeave mChannel reason = do
  st <- get
  case (ChatWithChannel <$> mChannel) <|> Just (appCurrentChat st) of
    Just chatID@(ChatWithChannel channel) -> do
      liftIO $ writeAction (appClient st) $ LeaveChannel channel reason
      modify $ removeChat chatID
    _ -> handleHelp

handleList :: EventM ViewportName AppState ()
handleList = get >>= \st -> liftIO $ writeAction (appClient st) ListChannels

handleNames :: EventM ViewportName AppState ()
handleNames = do
  st <- get
  case appCurrentChat st of
    ChatWithChannel channel -> do
      liftIO $ writeAction (appClient st) (ListMembers channel)
      modify $ updateChat (appCurrentChat st) $ \ch -> ch {chatMembers = mempty}
    _ -> pure ()

handleQuit :: Maybe Reason -> EventM ViewportName AppState ()
handleQuit mReason = do
  st <- get
  liftIO $ writeAction (appClient st) (Quit mReason)
  halt

handleSendMessage :: Text -> EventM ViewportName AppState ()
handleSendMessage "" = pure ()
handleSendMessage text = do
  st <- get
  case appCurrentChat st of
    ChatWithServer _ -> pure ()
    chatID@(ChatWithChannel channel) -> do
      let target = TargetChannel channel
      liftIO $ writeAction (appClient st) $ SendMessage target text
      ts <- liftIO getCurrentTime
      let chatMsg = ChatMessage ts (Just $ nickname $ appUser st) [text] Normal
      modify $ updateChat chatID $ appendChatMessage chatMsg
      scrollMessagesToEnd
    chatID@(ChatWithNickname nick) -> do
      liftIO $ writeAction (appClient st) $ SendMessage (TargetUser nick) text
      ts <- liftIO getCurrentTime
      let chatMsg = ChatMessage ts (Just $ nickname $ appUser st) [text] Normal
      modify $ updateChat chatID $ appendChatMessage chatMsg
      scrollMessagesToEnd

handleNotice :: Text -> EventM ViewportName AppState ()
handleNotice msg = case T.strip msg of
  "" -> pure ()
  text -> do
    st <- get
    case appCurrentChat st of
      ChatWithServer _ -> pure ()
      chatID@(ChatWithChannel channel) -> do
        let target = TargetChannel channel
        liftIO $ writeAction (appClient st) $ SendNotice target text
        ts <- liftIO getCurrentTime
        let chatMsg = ChatMessage ts (Just $ nickname $ appUser st) [text] Notice
        modify $ updateChat chatID $ appendChatMessage chatMsg
        scrollMessagesToEnd
      chatID@(ChatWithNickname nick) -> do
        liftIO $ writeAction (appClient st) $ SendNotice (TargetUser nick) text
        ts <- liftIO getCurrentTime
        let chatMsg = ChatMessage ts (Just $ nickname $ appUser st) [text] Notice
        modify $ updateChat chatID $ appendChatMessage chatMsg
        scrollMessagesToEnd

handlePrivateMessage :: Text -> Text -> EventM ViewportName AppState ()
handlePrivateMessage "" _ = pure ()
handlePrivateMessage _ "" = pure ()
handlePrivateMessage nickStr text = do
  let nick = Nickname nickStr
  st <- get
  liftIO $ writeAction (appClient st) $ SendMessage (TargetUser nick) text
  ts <- liftIO getCurrentTime
  let chatMsg = ChatMessage ts (Just $ nickname $ appUser st) [text] Normal
  modify $ updateChat (ChatWithNickname nick) $ appendChatMessage chatMsg
  scrollMessagesToEnd

handleQuery :: Nickname -> EventM ViewportName AppState ()
handleQuery nick = do
  modify $ \s ->
    let chatID = ChatWithNickname nick
        chat = newChat (fromList [nick, nickname $ appUser s])
        newChats = Map.insert chatID chat $ appChats s
     in s {appCurrentChat = chatID, appChats = newChats}
  scrollMessagesToEnd

handleCloseQuery :: EventM ViewportName AppState ()
handleCloseQuery = do
  AppState {..} <- get
  case appCurrentChat of
    chatID@(ChatWithNickname _) -> modify $ removeChat chatID
    _ -> pure ()

handleHelp :: EventM ViewportName AppState ()
handleHelp = do
  st <- get
  ts <- liftIO getCurrentTime
  let keybindingsText = keybindingTextTable keyConfig [("", keyEventHandlers)]
      helpMsg =
        [ "Commands",
          "========",
          "  /help                     - Show this help message",
          "  /join #channel            - Join a channel",
          "  /part                     - Leave the current channel",
          "  /names                    - List members in the current channel",
          "  /list                     - List available channels",
          "  /nick <nickname>          - Change your nickname",
          "  /notice <message>         - Send a notice to the current channel",
          "  /topic [#channel] <topic> - View or set the channel topic",
          "  /away [reason]            - Set yourself as away",
          "  /msg <nick> <message>     - Send a private message",
          "  /query <nick>             - Open a private conversation",
          "  /closequery               - Close the current private conversation",
          "  /quit [reason]            - Quit the application"
        ]
          <> T.lines keybindingsText
      chatMsg = ChatMessage ts Nothing helpMsg CommandReply
  modify $ updateChat (appCurrentChat st) $ appendChatMessage chatMsg
  scrollMessagesToEnd

handleNick :: Nickname -> EventM ViewportName AppState ()
handleNick nickname = do
  st <- get
  liftIO $ writeAction (appClient st) $ SetNickname nickname

handleAway :: Maybe Reason -> EventM ViewportName AppState ()
handleAway reason = do
  st <- get
  liftIO $ writeAction (appClient st) $ SetAway reason

handleTopic :: Maybe Text -> EventM ViewportName AppState ()
handleTopic mTopic = do
  AppState {..} <- get
  liftIO $ case appCurrentChat of
    ChatWithChannel channel -> writeAction appClient $ Topic channel mTopic
    _ -> pure ()
