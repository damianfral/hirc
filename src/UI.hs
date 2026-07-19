{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI (runUI) where

import Brick
import qualified Brick.AttrMap as A
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Edit (Editor, editorText, getEditContents, handleEditorEvent, renderEditor)
import Control.Concurrent.Async (async, cancel)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import IRC.Client
import IRC.Domain
import IRC.Protocol (Nickname (..), User (..))
import Network.Socket (HostName)
import Relude
import UI.AppState

uiApp :: App AppState Event ViewportName
uiApp =
  App
    { appDraw = viewUI,
      appChooseCursor = const $ showCursorNamed ChatInput,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const $ A.attrMap V.defAttr []
    }

handleEvent :: BrickEvent ViewportName Event -> EventM ViewportName AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = haltWithQuit
handleEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = haltWithQuit
handleEvent (VtyEvent (V.EvKey V.KPageUp [])) =
  vScrollPage (viewportScroll ChatViewport) Brick.Up
handleEvent (VtyEvent (V.EvKey V.KPageDown [])) =
  vScrollPage (viewportScroll ChatViewport) Brick.Down
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = handleEnter
handleEvent ev@(VtyEvent _) = do
  st <- get
  newEditor <- nestEventM' (appInput st) $ handleEditorEvent ev
  put $ st {appInput = newEditor}
handleEvent (AppEvent event) = do
  modify $ updateState event
  vScrollToEnd $ viewportScroll ChatViewport
handleEvent (MouseDown (Scrollable _element vp) V.BScrollUp _mods _location) =
  vScrollBy (viewportScroll vp) (-3)
handleEvent (MouseDown (Scrollable _element vp) V.BScrollDown _mods _location) =
  vScrollBy (viewportScroll vp) 3
handleEvent (MouseDown (Scrollable element vp) _button _mods _location) =
  case element of
    SBHandleBefore -> vScrollBy (viewportScroll vp) (-1)
    SBHandleAfter -> vScrollBy (viewportScroll vp) 1
    SBTroughBefore -> vScrollPage (viewportScroll vp) Brick.Up
    SBTroughAfter -> vScrollPage (viewportScroll vp) Brick.Down
    SBBar -> pure ()
handleEvent _ = pure ()

runUI :: HostName -> IRCClient -> User -> IO ()
runUI hostname client user = do
  vty <- buildVty
  bchan <- newBChan 256
  bchanLoopAsync <- liftIO $ async $ ircClientToBChanEventLoop client bchan
  void $ customMain vty buildVty (Just bchan) uiApp initialState
  cancel bchanLoopAsync
  where
    buildVty = do
      v <- mkVty V.defaultConfig
      V.setMode (V.outputIface v) V.Mouse True -- mouse support
      pure v
    initialState =
      AppState
        { appClient = client,
          appUser = user,
          appCurrentChannel = Nothing,
          appChannels = mempty,
          appHostMessages = [],
          appHost = toText hostname,
          appInput = emptyEditor
        }

emptyEditor :: Editor Text ViewportName
emptyEditor = editorText ChatInput Nothing ""

resetUserInput :: AppState -> AppState
resetUserInput st = st {appInput = emptyEditor}

viewMembers :: Set Nickname -> Widget ViewportName
viewMembers nicks = vBox $ txt . unNickname <$> toList nicks

viewChannelName :: Channel -> Widget n
viewChannelName = txt . channelToText

viewMessages :: [Text] -> Widget ViewportName
viewMessages msgs = withClickableVScrollBars Scrollable $ do
  withVScrollBars OnRight $ viewport ChatViewport Vertical $ vBox $ txt <$> msgs

viewUI :: AppState -> [Widget ViewportName]
viewUI AppState {..} = [vBox [mainWidget, chatBar]]
  where
    mainWidget = case appCurrentChannel of
      Nothing -> borderWithLabel (txt $ " " <> appHost <> " ") $ do
        viewMessages appHostMessages
      Just channel ->
        hBox
          [ borderWithLabel (viewChannelName channel)
              $ viewMessages
              $ fromMaybe [] currentChannelMessages,
            hLimit 20
              $ borderWithLabel (txt " members ")
              $ withClickableVScrollBars Scrollable
              $ withVScrollBars OnRight
              $ viewport ChatMembers Vertical
              $ viewMembers
              $ fromMaybe mempty currentChannelNicknames
          ]
    currentChannelMessages = case appCurrentChannel of
      Nothing -> Just appHostMessages
      Just chanName -> do
        uiChann <- Map.lookup chanName appChannels
        pure $ channelMessages uiChann
    currentChannelNicknames = do
      chanName <- appCurrentChannel
      uiChann <- Map.lookup chanName appChannels
      pure $ channelNicknames uiChann
    chatBar = vLimit 3 $ border $ hBox [str "> ", inputWidget]
    inputWidget = renderEditor viewEditorLines True appInput
    viewEditorLines = txt . T.unlines

ircClientToBChanEventLoop :: IRCClient -> BChan Event -> IO ()
ircClientToBChanEventLoop client bchan = loop
  where
    loop = do
      event <- readEvent client
      writeBChan bchan event
      case event of
        Disconnected _ -> pure ()
        _ -> loop

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

handleHelp :: EventM ViewportName AppState ()
handleHelp =
  modify
    $ appendServerMessage
    $ T.unlines
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
  _ -> modify $ appendServerMessage "Usage: /nick <nickname>"

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
    Nothing -> modify $ appendServerMessage "Usage: /topic [#channel] <topic>"
    Just action -> liftIO $ writeAction appClient action

--------------------------------------------------------------------------------
