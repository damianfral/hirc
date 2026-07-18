{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module IRC.UI (runUI) where

import Brick
import qualified Brick.AttrMap as A
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border (border, hBorder, vBorder)
import Control.Concurrent.Async (Async, async, cancel)
import Data.Map (lookup)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Graphics.Vty as V
import IRC.Client
import IRC.Domain
import IRC.Protocol (Nickname (..), User (..))
import Relude

data ChatViewport = ChatViewport deriving (Show, Ord, Eq)

data UIChannel = UIChannel
  { uiChannelMessages :: [Text],
    uiChannelNicknames :: Set Nickname
  }

data UIState = UIState
  { uiClient :: IRCClient,
    uiNickname :: Nickname,
    uiCurrentChannel :: Maybe Channel,
    uiChannels :: Map Channel UIChannel,
    uiServerMessages :: [Text],
    uiInput :: Text,
    uiConnected :: Bool,
    uiReader :: Maybe (Async ())
  }

runUI :: IRCClient -> Nickname -> IO ()
runUI client nick = do
  bchan <- newBChan 32
  let startEvent = do
        rd <- liftIO $ async $ ircClientToBChanEventLoop client bchan
        modify (\ui -> ui {uiReader = Just rd})
  let app =
        App
          { appDraw = drawUI,
            appChooseCursor = showFirstCursor,
            appHandleEvent = handleEvent,
            appStartEvent = startEvent,
            appAttrMap = const theAttrMap
          }
  (finalSt, _) <- customMainWithDefaultVty (Just bchan) app initialState
  forM_ (uiReader finalSt) cancel
  where
    initialState =
      UIState
        { uiClient = client,
          uiNickname = nick,
          uiCurrentChannel = Nothing,
          uiChannels = mempty,
          uiServerMessages = [],
          uiInput = "",
          uiConnected = True,
          uiReader = Nothing
        }
    handleEvent (VtyEvent (V.EvKey key modifiers)) =
      case (key, modifiers) of
        (V.KEsc, []) -> haltWithQuit
        (V.KChar 'c', [V.MCtrl]) -> haltWithQuit
        (V.KEnter, []) -> handleEnter
        (V.KBS, []) -> modify $ \ui -> ui {uiInput = T.dropEnd 1 (uiInput ui)}
        (V.KDel, []) -> modify $ \ui -> ui {uiInput = T.dropEnd 1 (uiInput ui)}
        (V.KChar c, []) -> modify $ \ui -> ui {uiInput = uiInput ui <> T.singleton c}
        _ -> pure ()
    handleEvent (AppEvent event) = do
      modify $ updateState event
      vScrollToEnd (viewportScroll ChatViewport)
    handleEvent _ = pure ()

theAttrMap :: A.AttrMap
theAttrMap = A.attrMap V.defAttr []

drawMembers :: Channel -> Set Nickname -> Widget ChatViewport
drawMembers channel nicks = hLimit 20 $ vBox $ do
  mconcat
    [ [txt $ renderChannelName channel],
      [hBorder],
      txt . unNickname <$> toList nicks,
      [fill ' ']
    ]

drawMessages :: [Text] -> Widget ChatViewport
drawMessages msgs =
  withVScrollBars OnRight $ viewport ChatViewport Vertical $ do
    vBox $ txt <$> msgs

drawUI :: UIState -> [Widget ChatViewport]
drawUI UIState {..} =
  [ vBox
      $ ( case uiCurrentChannel of
            Nothing -> [border $ drawMessages uiServerMessages]
            Just channel ->
              [ border
                  $ hBox
                    [ drawMessages $ fromMaybe [] currentChannelMessages,
                      vBorder,
                      drawMembers channel $ fromMaybe mempty currentChannelNicknames
                    ]
              ]
        )
      <> [vLimit 3 $ border $ hBox [str "> ", txt uiInput, fill ' ']]
  ]
  where
    currentChannelMessages = case uiCurrentChannel of
      Nothing -> Just uiServerMessages
      Just chanName -> do
        uiChann <- lookup chanName uiChannels
        pure $ uiChannelMessages uiChann
    currentChannelNicknames = do
      chanName <- uiCurrentChannel
      uiChann <- lookup chanName uiChannels
      pure $ uiChannelNicknames uiChann

renderChannelName :: Channel -> Text
renderChannelName (Channel c) = "#" <> c

ircClientToBChanEventLoop :: IRCClient -> BChan Event -> IO ()
ircClientToBChanEventLoop client bchan = loop
  where
    loop = do
      event <- readEvent client
      writeBChan bchan event
      case event of
        Disconnected _ -> pure ()
        _ -> loop

haltWithQuit :: EventM ChatViewport UIState ()
haltWithQuit = do
  UIState {..} <- get
  liftIO $ do
    writeAction uiClient (Quit Nothing)
    forM_ uiReader cancel
  halt

handleEnter :: EventM ChatViewport UIState ()
handleEnter = do
  ui@UIState {..} <- get
  case T.strip uiInput of
    "" -> pure ()
    msg
      | "/join" `T.isPrefixOf` msg -> handleJoin msg
      | "/list" `T.isPrefixOf` msg -> handleList
      | "/quit" `T.isPrefixOf` msg -> handleQuit msg
      | otherwise -> handleSend ui msg

  modify $ \s -> s {uiInput = ""}

handleJoin :: Text -> EventM ChatViewport UIState ()
handleJoin msg = case T.words msg of
  [_cmd, ch] -> do
    let channel = Channel (T.dropWhile (== '#') ch)
    ui <- get
    liftIO $ writeAction (uiClient ui) $ JoinChannel channel
    let newUIChannel =
          UIChannel
            { uiChannelMessages = mempty,
              uiChannelNicknames = mempty
            }
    modify $ \ui' ->
      ui'
        { uiChannels = Map.insert channel newUIChannel (uiChannels ui'),
          uiCurrentChannel = Just channel
        }
  _ -> pure ()

handleList :: EventM ChatViewport UIState ()
handleList = get >>= \ui -> liftIO $ writeAction (uiClient ui) ListChannels

handleQuit :: Text -> EventM ChatViewport UIState ()
handleQuit msg = do
  let reason = case T.words msg of
        [_cmd, r] -> Just (Reason r)
        _ -> Nothing
  UIState {..} <- get
  liftIO $ forM_ uiReader cancel
  liftIO $ writeAction uiClient (Quit reason)
  halt

handleSend :: UIState -> Text -> EventM ChatViewport UIState ()
handleSend ui text = case uiCurrentChannel ui of
  Nothing -> pure ()
  Just ch -> do
    let target = TargetChannel ch
    liftIO $ writeAction (uiClient ui) $ SendMessage target text

updateState :: Event -> UIState -> UIState
updateState (Connected server _welcome) =
  appendServerMessages ["Connected to " <> show server]
updateState (MessageReceived user (TargetChannel channel) msg) =
  appendMessageToUIChannel [nickOf user <> ": " <> msg] channel
updateState (NoticeReceived user (TargetChannel channel) msg) =
  appendMessageToUIChannel ["[NOTICE] " <> nickOf user <> ": " <> msg] channel
updateState (UserJoined user channel) =
  let msg = "--> " <> nickOf user <> " joined"
   in modifyUIChannel channel $ \uiChannel@UIChannel {..} ->
        uiChannel
          { uiChannelMessages = uiChannelMessages <> [msg],
            uiChannelNicknames = Set.insert (nickname user) uiChannelNicknames
          }
updateState (UserLeft user channel _reason) =
  appendMessageToUIChannel ["<-- " <> nickOf user <> " left"] channel
    . removeNicknameFromChannel (nickname user) channel
updateState (NickChanged user n@(Nickname nick)) =
  appendServerMessages [nickOf user <> " is now known as " <> nick]
    . modifyUserNick user n
updateState (UserDisconnected user _reason) =
  appendServerMessages ["<-- " <> nickOf user <> " quit"]
    . removeNicknameFromAllChannels (nickname user)
updateState (ChannelUsers channel nicks) = addNicknamesToChannel nicks channel
updateState (ChannelListEntry channel count topic) =
  appendMessageToUIChannel ["[LIST] " <> channelShow channel <> " (" <> show count <> " users) " <> topic] channel
updateState (Disconnected reason) = appendServerMessages ["Disconnected: " <> reason]
updateState _ = id

--------------------------------------------------------------------------------

modifyUIChannel :: Channel -> (UIChannel -> UIChannel) -> UIState -> UIState
modifyUIChannel channel update ui =
  ui {uiChannels = Map.adjust update channel (uiChannels ui)}

appendMessageToUIChannel :: [Text] -> Channel -> UIState -> UIState
appendMessageToUIChannel msg channel =
  modifyUIChannel channel $ \uiChannel ->
    uiChannel {uiChannelMessages = uiChannelMessages uiChannel <> msg}

appendServerMessages :: [Text] -> UIState -> UIState
appendServerMessages msgs ui =
  ui {uiServerMessages = uiServerMessages ui <> msgs}

addNicknamesToChannel :: Set Nickname -> Channel -> UIState -> UIState
addNicknamesToChannel users channel = modifyUIChannel channel $ \uiChannel ->
  uiChannel {uiChannelNicknames = users <> uiChannelNicknames uiChannel}

removeNicknameFromChannel :: Nickname -> Channel -> UIState -> UIState
removeNicknameFromChannel user channel = modifyUIChannel channel $ \uiChannel ->
  uiChannel {uiChannelNicknames = Set.delete user $ uiChannelNicknames uiChannel}

removeNicknameFromAllChannels :: Nickname -> UIState -> UIState
removeNicknameFromAllChannels nick = modifyAllUIChannels $ \uiChannel ->
  uiChannel {uiChannelNicknames = Set.delete nick $ uiChannelNicknames uiChannel}

modifyAllUIChannels :: (UIChannel -> UIChannel) -> UIState -> UIState
modifyAllUIChannels update ui =
  ui {uiChannels = update <$> uiChannels ui}

modifyUserNick :: User -> Nickname -> UIState -> UIState
modifyUserNick user newNick = modifyAllUIChannels $ \uiChannel ->
  uiChannel {uiChannelNicknames = Set.delete oldNick $ Set.insert newNick $ uiChannelNicknames uiChannel}
  where
    oldNick = nickname user

--------------------------------------------------------------------------------

channelShow :: Channel -> Text
channelShow (Channel c)
  | "#" `T.isPrefixOf` c = c
  | otherwise = "#" <> c

nickOf :: User -> Text
nickOf = (\(Nickname n) -> n) . nickname
