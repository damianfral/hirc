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
import UI.Style (nicknameToColorAttr, noticeAttrName)

padX :: Int -> Widget n -> Widget n
padX x = padLeft (Pad x) . padRight (Pad x)

viewChannels :: Map Channel ChannelState -> ConversationView -> Widget ViewportName
viewChannels chans currentView = vBox $ if Map.null chans then [] else names
  where
    names =
      [ let isSelected = currentView == ChannelView k
            w = txt $ channelToText k
         in if isSelected then w else withAttr noticeAttrName w
      | (k, _v) <- Map.toList chans
      ]

viewMembers :: Set Nickname -> Widget ViewportName
viewMembers nicks = withVScrollBars OnRight $ do
  viewport ChannelMembers Vertical $ vBox $ viewNickname <$> toList nicks

viewChannelName :: Channel -> Widget n
viewChannelName = txt . channelToText

viewChatMessages ::
  Either Server Channel -> [ChatMessage] -> Widget ViewportName
viewChatMessages from msgs =
  borderWithLabel (padX 1 title) $ withVScrollBars OnRight $ do
    viewport Messages Vertical $ vBox $ viewChatMessage <$> msgs
  where
    title = case from of
      Left (Server server) -> txt server
      Right channel -> viewChannelName channel

viewChatMessage :: ChatMessage -> Widget ViewportName
viewChatMessage (ChatMessage ts Nothing msg tag) = withTagAttrs tag $ do
  hBox [viewTime ts, padLeft (Pad 1) $ viewChatMessageContent msg]
viewChatMessage (ChatMessage ts (Just nick) msg tag) = withTagAttrs tag $ do
  hBox [viewTime ts, padX 1 $ viewNickname nick, viewChatMessageContent msg]

withTagAttrs :: Tag -> Widget n -> Widget n
withTagAttrs t = withAttr (attrName (show t))

-- withTagAttrs _ w = w

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
viewUI AppState {..} = [vBox [mainWidget, chatBar]]
  where
    channelListWidget =
      hLimit 24 $ borderWithLabel (txt " channels ") $ do
        withVScrollBars OnRight $ viewport Channels Vertical $ do
          viewChannels appChannels appConversationView
    mainWidget = case appConversationView of
      ServerView -> hBox [channelListWidget, viewChatMessages (Left appServer) appHostMessages]
      ChannelView channel -> do
        let msgs = fromMaybe [] currentChannelMessages
        hBox
          [ channelListWidget,
            viewChatMessages (Right channel) msgs,
            hLimit 24 $ borderWithLabel (padX 1 $ txt "members") $ do
              viewMembers $ fromMaybe mempty currentChannelNicknames
          ]
    currentChannelMessages = case appConversationView of
      ServerView -> Just appHostMessages
      ChannelView chanName -> do
        uiChann <- Map.lookup chanName appChannels
        pure $ channelMessages uiChann
    currentChannelNicknames = case appConversationView of
      ServerView -> Nothing
      ChannelView chanName -> do
        uiChann <- Map.lookup chanName appChannels
        pure $ channelNicknames uiChann
    chatBar =
      vLimit (2 + length (getEditContents appInput)) $ border $ hBox $ do
        [viewNickname $ nickname appUser, padLeft (Pad 1) inputWidget]
    inputWidget = renderEditor viewEditorLines True appInput
    viewEditorLines = txt . T.unlines
