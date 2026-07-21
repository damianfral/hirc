{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.EventHandler where

import Brick
import qualified Brick.Keybindings.KeyDispatcher as KD
import Brick.Widgets.Edit (getEditContents, handleEditorEvent)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Graphics.Vty as V
import IRC.Client
import IRC.Domain
import IRC.Protocol (Nickname (..), User (..))
import Relude
import UI.AppState
import UI.KeyEvent

handleEvent :: BrickEvent ViewportName Event -> EventM ViewportName AppState ()
handleEvent (VtyEvent k@(V.EvKey key mods)) = do
  handled <- dispatcher
  when (not handled) $ do
    st <- get
    newEditor <- nestEventM' (appInput st) $ handleEditorEvent (VtyEvent k)
    put $ st {appInput = newEditor}
  where
    dispatcher = case keyDispatcher of
      Nothing -> pure False
      Just kd -> KD.handleKey kd key mods
handleEvent (VtyEvent _) = pure ()
handleEvent (AppEvent event) = do
  modify $ updateState event
  scrollMessagesToEnd
handleEvent (MouseDown vp direction _mods _location) = do
  let scrollLines = case direction of
        V.BScrollUp -> (-4)
        V.BScrollDown -> 4
        _ -> 0
  vScrollBy (viewportScroll vp) scrollLines
handleEvent (MouseUp {}) = pure ()

scrollMessagesToEnd :: EventM ViewportName s ()
scrollMessagesToEnd = vScrollToEnd $ viewportScroll Messages

handleScroll :: Direction -> EventM ViewportName s ()
handleScroll = vScrollPage (viewportScroll Messages)

-- TODO: we are ignoring conflicting keybindings
keyDispatcher :: Maybe (KD.KeyDispatcher KeyEvent (EventM ViewportName AppState))
keyDispatcher = case KD.keyDispatcher keyConfig keyEventHandlers of
  Right d -> Just d
  Left _conflictingKeybindings -> Nothing

keyEventHandlers :: [KD.KeyEventHandler KeyEvent (EventM ViewportName AppState)]
keyEventHandlers =
  [ KD.onEvent EvQuit "Quit the application" haltWithQuit,
    KD.onEvent EvScrollUp "Scroll messages up" $ handleScroll Brick.Up,
    KD.onEvent EvScrollDown "Scroll messages down" $ handleScroll Brick.Down,
    KD.onEvent EvNextChannel "Go to next channel" $ do
      modify goToNextChannel >> scrollMessagesToEnd,
    KD.onEvent EvPrevChannel "Go to previous channel" $ do
      modify goToPrevChannel >> scrollMessagesToEnd,
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
  case content of
    "" -> pure ()
    "/help" -> handleHelp
    "/part" -> handleLeave
    "/names" -> handleNames
    "/list" -> handleList
    msg
      | "/join" `T.isPrefixOf` msg -> handleJoin msg
      | "/nick" `T.isPrefixOf` msg -> handleNick msg
      | "/away" `T.isPrefixOf` msg -> handleAway msg
      | "/topic" `T.isPrefixOf` msg -> handleTopic msg
      | "/quit" `T.isPrefixOf` msg -> handleQuit msg
      | otherwise -> handleSendMessage msg
  modify resetUserInput

handleJoin :: Text -> EventM ViewportName AppState ()
handleJoin msg = case T.words msg of
  [_cmd, ch] -> do
    let channel = Channel (T.dropWhile (== '#') ch)
    st <- get
    when (Map.notMember channel (appChannels st)) $ do
      liftIO $ writeAction (appClient st) $ JoinChannel channel
    let newChannelState = ChannelState mempty mempty
    modify $ \st' ->
      let newChannels = Map.insert channel newChannelState (appChannels st')
       in st' {appChannels = newChannels, appCurrentChannel = Just channel}
  _ -> pure ()

handleLeave :: EventM ViewportName AppState ()
handleLeave = do
  st <- get
  case appCurrentChannel st of
    Nothing -> pure ()
    Just channel -> do
      liftIO $ writeAction (appClient st) $ LeaveChannel channel Nothing
      let newChannelState = ChannelState mempty mempty
      modify $ \st' ->
        let newChannels = Map.insert channel newChannelState (appChannels st')
         in st' {appChannels = newChannels, appCurrentChannel = Nothing}

handleList :: EventM ViewportName AppState ()
handleList = get >>= \st -> liftIO $ writeAction (appClient st) ListChannels

handleNames :: EventM ViewportName AppState ()
handleNames = do
  st <- get
  case appCurrentChannel st of
    Nothing -> pure ()
    Just channel -> liftIO $ writeAction (appClient st) (ListMembers channel)

handleQuit :: Text -> EventM ViewportName AppState ()
handleQuit msg = do
  let reason = case T.words msg of
        [_cmd, r] -> Just (Reason r)
        _ -> Nothing
  AppState {..} <- get
  liftIO $ writeAction appClient (Quit reason)
  halt

handleSendMessage :: Text -> EventM ViewportName AppState ()
handleSendMessage "" = pure ()
handleSendMessage text = do
  st <- get
  case appCurrentChannel st of
    Nothing -> pure ()
    Just channel -> do
      let target = TargetChannel channel
      liftIO $ writeAction (appClient st) $ SendMessage target text
      -- Since we have not implemented echo-message, just append the message.
      modify
        $ appendChatMessage
          (ChatMessage (Just $ nickname $ appUser st) [text] Normal)
          channel
      scrollMessagesToEnd

handleHelp :: EventM ViewportName AppState ()
handleHelp = do
  st <- get
  let chatMsg = ChatMessage Nothing helpMsg Dimmed
  modify $ case appCurrentChannel st of
    Nothing -> appendServerChatMessage $ ChatMessage Nothing helpMsg Dimmed
    Just channel -> appendChatMessage chatMsg channel
  scrollMessagesToEnd
  where
    helpMsg =
      [ "Available commands:",
        "  /help            - Show this help message",
        "  /join #channel   - Join a channel",
        "  /part            - Leave the current channel",
        "  /names           - List members in the current channel",
        "  /list            - List available channels",
        "  /nick <nickname> - Change your nickname",
        "  /topic [#channel] <topic> - View or set the channel topic",
        "  /away [reason]   - Set yourself as away",
        "  /quit [reason]   - Quit the application"
      ]

handleNick :: Text -> EventM ViewportName AppState ()
handleNick msg = case T.words msg of
  [_cmd, nick] -> do
    st <- get
    liftIO $ writeAction (appClient st) $ SetNickname (Nickname nick)
  _ -> do
    let chatMsg = ChatMessage Nothing ["Usage: /nick <nickname>"] Dimmed
    modify $ appendServerChatMessage chatMsg

handleAway :: Text -> EventM ViewportName AppState ()
handleAway msg = do
  let reason = case T.words msg of
        [_cmd, r] -> Just (Reason r)
        _ -> Nothing
  st <- get
  liftIO $ writeAction (appClient st) $ SetAway reason

handleTopic :: Text -> EventM ViewportName AppState ()
handleTopic msg = do
  AppState {..} <- get
  let mAction =
        case T.words msg of
          [_cmd, ch, t] ->
            let channel = Channel (T.dropWhile (== '#') ch)
             in Just $ Topic channel (Just t)
          [_cmd, t] -> case appCurrentChannel of
            Nothing -> Nothing
            Just channel -> Just $ Topic channel (Just t)
          [_cmd] -> case appCurrentChannel of
            Nothing -> Nothing
            Just channel -> Just $ Topic channel Nothing
          _ -> Nothing
  case mAction of
    Nothing -> do
      let chatMsg =
            ChatMessage Nothing ["Usage: /topic [#channel] <topic>"] Dimmed
      modify $ appendServerChatMessage chatMsg
    Just action -> liftIO $ writeAction appClient action
