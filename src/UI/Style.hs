{-# LANGUAGE NoImplicitPrelude #-}

module UI.Style where

import Brick
import qualified Data.Text as T
import Graphics.Vty
import qualified Graphics.Vty.Attributes as V
import IRC.Protocol (Nickname (..))
import Relude
import UI.Chat (Tag (..))

attributes :: [(AttrName, Attr)]
attributes =
  [ (noticeAttrName, V.defAttr `V.withStyle` V.dim),
    (commandReplyAttrName, V.defAttr `V.withForeColor` V.yellow),
    (infoAttrName, V.defAttr `V.withForeColor` V.blue),
    ( serverEventAttrName,
      V.defAttr `V.withStyle` V.dim `V.withForeColor` V.yellow
    )
  ]
    <> do
      (i, color) <- zip [0 ..] nicknameColors
      [(nicknameColorAttr i, V.defAttr `V.withForeColor` color)]

infoAttrName :: AttrName
infoAttrName = attrName $ show Info

channelNotSelectedAttrName :: AttrName
channelNotSelectedAttrName = attrName "ChannelNotSelected"

noticeAttrName :: AttrName
noticeAttrName = attrName $ show Notice

commandReplyAttrName :: AttrName
commandReplyAttrName = attrName $ show CommandReply

serverEventAttrName :: AttrName
serverEventAttrName = attrName $ show ServerEvent

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
