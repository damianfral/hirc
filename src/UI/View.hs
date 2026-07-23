{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.View where

import Brick
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Edit (getEditContents, renderEditor)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import IRC.Domain
import IRC.Protocol (Nickname (..), Server (..), User (..))
import Relude
import UI.AppState
import UI.Chat
import UI.Style (nicknameToColorAttr, noticeAttrName)

padX :: Int -> Widget n -> Widget n
padX x = padLeft (Pad x) . padRight (Pad x)

-- TODO: Unread count
viewChannels :: AppState -> Widget ViewportName
viewChannels AppState {..} = vBox $ do
  let chatIDs = Map.keys appChats
  if appChats == mempty
    then []
    else (\cid -> viewChatID (cid == appCurrentChat) cid) <$> chatIDs

viewChatID :: Bool -> ChatID -> Widget n
viewChatID _ (ChatWithServer (Server _)) = emptyWidget
viewChatID isCurrent (ChatWithChannel (Channel c)) =
  stylize isCurrent $ txt $ "#" <> c
viewChatID isCurrent (ChatWithNickname nick@(Nickname n)) =
  withAttr (nicknameToColorAttr nick) $ stylize isCurrent $ txt $ "@" <> n

stylize :: Bool -> Widget n -> Widget n
stylize True = id
stylize False = withAttr noticeAttrName

viewMembers :: Set Nickname -> Widget ViewportName
viewMembers nicks = withVScrollBars OnRight $ do
  viewport ChannelMembers Vertical $ vBox $ viewNickname <$> toList nicks

viewChatMessages :: ChatID -> [ChatMessage] -> Widget ViewportName
viewChatMessages chatID msgs =
  borderWithLabel (padX 1 (viewChatID True chatID)) $ do
    withVScrollBars OnRight $ do
      viewport Messages Vertical $ vBox $ viewChatMessage <$> msgs

viewChatMessage :: ChatMessage -> Widget ViewportName
viewChatMessage (ChatMessage ts Nothing msg tag) = withTagAttrs tag $ do
  hBox [viewTime ts, padLeft (Pad 1) $ viewChatMessageContent msg]
viewChatMessage (ChatMessage ts (Just nick) msg tag) = withTagAttrs tag $ do
  hBox [viewTime ts, padX 1 $ viewNickname nick, viewChatMessageContent msg]

withTagAttrs :: Tag -> Widget n -> Widget n
withTagAttrs t = withAttr (attrName (show t))

viewChatMessageContent :: [Text] -> Widget n
viewChatMessageContent = vBox . fmap txtWrap

viewNickname :: Nickname -> Widget n
viewNickname nick@(Nickname nickStr) =
  withAttr (nicknameToColorAttr nick) $ hLimit (T.length nickStr + 4) $ do
    withAttr (nicknameToColorAttr nick) $ txt $ "<" <> nickStr <> ">"

viewTime :: UTCTime -> Widget n
viewTime ts = hLimit 7 $ txt $ "[" <> tsText <> "]"
  where
    tsText = T.pack $ formatTime defaultTimeLocale "%H:%M" ts

viewUI :: AppState -> [Widget ViewportName]
viewUI st@AppState {..} = [vBox [mainWidget, chatBar]]
  where
    Server s = appServer
    channelListWidget =
      hLimit 24 $ borderWithLabel (txt $ " " <> s <> " ") $ do
        withVScrollBars OnRight $ viewport Channels Vertical $ do
          viewChannels st
    viewMessages =
      viewChatMessages appCurrentChat
        $ maybe [] chatMessages (Map.lookup appCurrentChat appChats)
    mainWidget = case appCurrentChat of
      ChatWithServer _server -> hBox [channelListWidget, viewMessages]
      _ -> do
        hBox
          [ channelListWidget,
            viewMessages,
            hLimit 24 $ borderWithLabel (padX 1 $ txt "members") $ do
              viewMembers $ do
                maybe mempty chatMembers (Map.lookup appCurrentChat appChats)
          ]
    chatBar =
      vLimit (2 + length (getEditContents appInput)) $ border $ hBox $ do
        [viewNickname $ nickname appUser, padLeft (Pad 1) inputWidget]
    inputWidget = renderEditor viewEditorLines True appInput
    viewEditorLines = txt . T.unlines
