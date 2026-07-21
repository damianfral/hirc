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
import Graphics.Vty
import IRC.Domain
import IRC.Protocol (Nickname (..), User (..))
import Relude
import UI.AppState

channelNotSelectedAttr :: AttrName
channelNotSelectedAttr = attrName "channelNotSelected"

dimmedAttr :: AttrName
dimmedAttr = attrName "dimmed"

nicknameColors :: [Color]
nicknameColors =
  [brightRed, brightGreen, brightYellow, brightBlue, brightMagenta, brightCyan]

nicknameColorAttr :: Int -> AttrName
nicknameColorAttr i = attrName $ "nicknameColor" <> show i

nicknameHash :: Nickname -> Int
nicknameHash (Nickname nick) = T.foldl' (\h c -> h * 31 + fromEnum c) 0 nick

nicknameToColorAttr :: Nickname -> AttrName
nicknameToColorAttr nick =
  nicknameColorAttr $ nicknameHash nick `mod` length nicknameColors

padX :: Int -> Widget n -> Widget n
padX x = padLeft (Pad x) . padRight (Pad x)

viewChannels :: Map Channel ChannelState -> Maybe Channel -> Widget ViewportName
viewChannels chans current = vBox $ if Map.null chans then [] else names
  where
    names =
      [ let isSelected = Just k == current
            w = txt $ channelToText k
         in if isSelected then w else withAttr channelNotSelectedAttr w
      | (k, _v) <- Map.toList chans
      ]

viewMembers :: Set Nickname -> Widget ViewportName
viewMembers nicks = vBox $ viewColoredNickname <$> toList nicks
  where
    viewColoredNickname nick =
      withAttr (nicknameToColorAttr nick) $ txt $ unNickname nick

viewChannelName :: Channel -> Widget n
viewChannelName = txt . channelToText

viewChatMessages :: [ChatMessage] -> Widget ViewportName
viewChatMessages msgs = withVScrollBars OnRight $ do
  viewport Messages Vertical $ vBox $ viewChatMessage <$> msgs

viewChatMessage :: ChatMessage -> Widget ViewportName
viewChatMessage (ChatMessage ts Nothing msg tag) = withTagAttrs tag $ do
  hBox [viewTime ts, padLeft (Pad 1) $ viewChatMessageContent msg]
viewChatMessage (ChatMessage ts (Just nick) msg tag) = withTagAttrs tag $ do
  hBox [viewTime ts, padX 1 $ viewNickname nick, viewChatMessageContent msg]

withTagAttrs :: Tag -> Widget n -> Widget n
withTagAttrs Dimmed w = withAttr dimmedAttr w
withTagAttrs _ w = w

viewChatMessageContent :: [Text] -> Widget n
viewChatMessageContent = vBox . fmap txtWrap

viewNickname :: Nickname -> Widget n
viewNickname nick =
  let (Nickname nickStr) = nick
   in hLimit (T.length nickStr + 4) $ withAttr (nicknameToColorAttr nick) $ do
        txt $ "<" <> nickStr <> ">"

viewTime :: UTCTime -> Widget n
viewTime ts =
  let tsStr = T.pack $ formatTime defaultTimeLocale "%H:%M" ts
   in hLimit 7 $ txt $ "[" <> tsStr <> "]"

viewUI :: AppState -> [Widget ViewportName]
viewUI AppState {..} = [vBox [mainWidget, chatBar]]
  where
    channelListWidget =
      hLimit 20 $ borderWithLabel (txt " channels ") $ do
        withVScrollBars OnRight $ viewport Channels Vertical $ do
          viewChannels appChannels appCurrentChannel
    mainWidget = case appCurrentChannel of
      Nothing ->
        hBox
          [ channelListWidget,
            borderWithLabel (padX 1 $ txt appHost)
              $ viewChatMessages appHostMessages
          ]
      Just channel ->
        hBox
          [ channelListWidget,
            borderWithLabel (padX 1 $ viewChannelName channel) $ do
              viewChatMessages $ fromMaybe [] currentChannelMessages,
            hLimit 20 $ borderWithLabel (padX 1 $ txt "members") $ do
              withVScrollBars OnRight $ viewport ChannelMembers Vertical $ do
                viewMembers $ fromMaybe mempty currentChannelNicknames
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
        [viewNickname $ nickname appUser, padLeft (Pad 1) inputWidget]
    inputWidget = renderEditor viewEditorLines True appInput
    viewEditorLines = txt . T.unlines
