{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UI.View where

import Brick
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Edit (getEditContents, renderEditor)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import IRC.Domain
import IRC.Protocol (Nickname (..), User (..))
import Relude
import UI.AppState

channelSelectedAttr :: AttrName
channelSelectedAttr = attrName "channelSelected"

dimmedAttr :: AttrName
dimmedAttr = attrName "dimmed"

viewChannels :: Map Channel ChannelState -> Maybe Channel -> Widget ViewportName
viewChannels chans current = vBox $ if Map.null chans then [] else names
  where
    names =
      [ let isSelected = Just k == current
            name = "  " <> channelToText k
            w = txt name
         in if isSelected then withAttr channelSelectedAttr w else w
      | (k, _v) <- Map.toList chans
      ]

viewMembers :: Set Nickname -> Widget ViewportName
viewMembers nicks = vBox $ txt . unNickname <$> toList nicks

viewChannelName :: Channel -> Widget n
viewChannelName = txt . channelToText

viewChatMessages :: [ChatMessage] -> Widget ViewportName
viewChatMessages msgs = withVScrollBars OnRight $ do
  viewport Messages Vertical $ vBox $ viewChatMessage <$> msgs

viewChatMessage :: ChatMessage -> Widget ViewportName
viewChatMessage (ChatMessage ts Nothing msg tag) = withTagAttrs tag $ do
  hBox [viewTime ts, txt " ", viewChatMessageContent msg]
viewChatMessage (ChatMessage ts (Just nick) msg tag) = withTagAttrs tag $ do
  hBox [viewTime ts, viewNickname nick, viewChatMessageContent msg]

withTagAttrs :: Tag -> Widget n -> Widget n
withTagAttrs tag w = case tag of
  Dimmed -> withAttr dimmedAttr w
  _ -> w

viewChatMessageContent :: [Text] -> Widget n
viewChatMessageContent = vBox . fmap txtWrap

viewNickname :: Nickname -> Widget n
viewNickname (Nickname nick) = hLimit (T.length nick + 4) $ do
  txt $ " <" <> nick <> "> "

viewTime :: UTCTime -> Widget n
viewTime ts =
  let tsStr = T.pack $ formatTime defaultTimeLocale "%H:%M" ts
   in hLimit 7 $ txt $ "[" <> tsStr <> "]"

viewUI :: AppState -> [Widget ViewportName]
viewUI AppState {..} = [vBox [mainWidget, chatBar]]
  where
    channelListWidget =
      hLimit 20 $
        borderWithLabel (txt " channels ") $
          withVScrollBars OnRight $
            viewport Channels Vertical $
              viewChannels appChannels appCurrentChannel
    mainWidget = case appCurrentChannel of
      Nothing ->
        hBox
          [ channelListWidget,
            borderWithLabel (txt $ " " <> appHost <> " ") $
              viewChatMessages appHostMessages
          ]
      Just channel ->
        hBox
          [ channelListWidget,
            borderWithLabel (viewChannelName channel) $
              viewChatMessages $
                fromMaybe [] currentChannelMessages,
            hLimit 20 $
              borderWithLabel (txt " members ") $
                withVScrollBars OnRight $
                  viewport ChannelMembers Vertical $
                    viewMembers $
                      fromMaybe mempty currentChannelNicknames
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
    chatBar =
      vLimit (2 + length (getEditContents appInput)) $ border $ hBox $ do
        [viewNickname $ nickname appUser, inputWidget]
    inputWidget = renderEditor viewEditorLines True appInput
    viewEditorLines = txt . T.unlines
