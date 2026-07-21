{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UI.View where

import Brick
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Edit (renderEditor)
import qualified Data.Map as Map
import qualified Data.Text as T
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
viewChatMessage (ChatMessage Nothing msg tag) =
  let w = vBox $ txt <$> msg
   in case tag of
        Dimmed -> withAttr dimmedAttr w
        _ -> w
viewChatMessage (ChatMessage (Just (Nickname nick)) msg tag) =
  let w = hBox [txtWrap $ T.concat $ [nick, ": "] <> msg]
   in case tag of
        Dimmed -> withAttr dimmedAttr w
        _ -> w

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
    chatBar = vLimit 3 $ border $ hBox $ do
      [str $ toString $ (nickname appUser & unNickname) <> ": ", inputWidget]
    inputWidget = renderEditor viewEditorLines True appInput
    viewEditorLines = txt . T.unlines
