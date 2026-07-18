{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module IRC.Domain where

import Data.GenValidity
import Data.GenValidity.Text ()
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import IRC.Protocol
import Relude hiding (atomically)

-- | Application layer
data Action
  = Register Nickname Username Realname
  | JoinChannel Channel
  | LeaveChannel Channel (Maybe Reason)
  | SendMessage Target Text
  | SendNotice Target Text
  | Quit (Maybe Reason)
  | Shutdown (Maybe Reason)
  | ListChannels
  deriving (Show, Eq)

newtype Realname = Realname Text
  deriving (Show, Eq, Generic)

newtype Channel = Channel Text
  deriving (Show, Eq, Generic, Ord)

newtype Reason = Reason Text
  deriving (Show, Eq, Generic)

data Target = TargetChannel Channel | TargetUser Nickname
  deriving (Show, Eq, Generic)

data Event
  = Connected Server Text
  | UserJoined User Channel
  | UserLeft User Channel (Maybe Reason)
  | MessageReceived User Target Text
  | NoticeReceived User Target Text
  | NickChanged User Nickname
  | UserDisconnected User (Maybe Reason)
  | ChannelUsers Channel (Set Nickname)
  | ChannelListEntry Channel Int Text
  | Disconnected Text
  deriving (Show, Eq)

actionToMessages :: Action -> [Message]
actionToMessages
  (Register (Nickname nick) (Username user) (Realname real)) =
    [ Message Nothing NICK (Params [nick]),
      let params' = Params [user, "0", "*", real]
       in Message Nothing USER params'
    ]
actionToMessages (JoinChannel (Channel channel)) =
  [Message Nothing JOIN (Params ["#" <> channel])]
actionToMessages (LeaveChannel (Channel channel) mReason) =
  let Params reasonParams = maybeReasonToParams mReason
      params' = Params $ "#" <> channel : reasonParams
   in [Message Nothing PART params']
actionToMessages (SendMessage target msg) =
  let params' = targetToParams target <> Params [msg]
   in [Message Nothing PRIVMSG params']
actionToMessages (SendNotice target msg) =
  let params' = targetToParams target <> Params [msg]
   in [Message Nothing NOTICE params']
actionToMessages (Quit mReason) =
  [Message Nothing QUIT $ maybeReasonToParams mReason]
actionToMessages (Shutdown reason) =
  [Message Nothing QUIT $ maybeReasonToParams reason]
actionToMessages ListChannels = [Message Nothing LIST mempty]

targetToParams :: Target -> Params
targetToParams target = Params $ case target of
  TargetChannel (Channel c) -> ["#" <> c]
  TargetUser (Nickname n) -> [n]

maybeReasonToParams :: Maybe Reason -> Params
maybeReasonToParams = maybe (Params []) reasonToParams

reasonToParams :: Reason -> Params
reasonToParams (Reason r) = Params [r]

messageToEvent :: Message -> Maybe Event
messageToEvent
  (Message (Just (PrefixServer server)) (Numeric 1) (Params (_ : welc : _))) =
    Just $ Connected server welc
messageToEvent (Message (Just (PrefixUser u)) JOIN (Params [channel])) =
  Just $ UserJoined u (Channel channel)
messageToEvent (Message (Just (PrefixUser u)) PART (Params (ch : reason))) =
  Just $ UserLeft u (Channel ch) (Reason <$> listToMaybe reason)
messageToEvent (Message (Just (PrefixUser u)) PRIVMSG (Params (target : msg))) =
  Just $ MessageReceived u (parseTarget target) (T.unwords msg)
messageToEvent (Message (Just (PrefixUser u)) NOTICE (Params (target : msg))) =
  Just $ NoticeReceived u (parseTarget target) (T.unwords msg)
messageToEvent (Message (Just (PrefixUser u)) NICK (Params [newNick])) =
  Just $ NickChanged u (Nickname newNick)
messageToEvent (Message (Just (PrefixUser u)) QUIT (Params (reason : _))) =
  Just $ UserDisconnected u (Reason <$> Just reason)
messageToEvent
  (Message (Just (PrefixServer _)) (Numeric 353) (Params (_ : _ : ch : us))) =
    let channel = Channel $ T.dropWhile (== '#') ch
     in Just $ ChannelUsers channel (parseNames $ T.unwords us)
messageToEvent
  (Message (Just (PrefixServer _)) (Numeric 322) (Params (_ : ch : num : topic))) =
    case TR.decimal num of
      Right (n, _) ->
        let channel = Channel $ T.dropWhile (== '#') ch
         in Just $ ChannelListEntry channel n (T.unwords topic)
      Left _ -> Nothing
messageToEvent _ = Nothing

parseNames :: Text -> Set Nickname
parseNames = fromList . mapMaybe parseNick . T.words
  where
    specialChars = "@+~%&!" :: String
    parseNick t = case T.uncons t of
      Just (c, rest) | c `elem` specialChars -> Just (Nickname rest)
      _ -> Just (Nickname t)

parseTarget :: Text -> Target
parseTarget target
  | "#" `T.isPrefixOf` target = TargetChannel (Channel $ T.drop 1 target)
  | otherwise = TargetUser (Nickname target)

instance Validity Realname

instance GenValid Realname

instance Validity Channel where
  validate (Channel t) =
    checkNotNull t "Channel is empty"
      <> checkNoLinebreak t "Channel contains control chars"

instance GenValid Channel

instance Validity Reason

instance GenValid Reason

instance Validity Target

instance GenValid Target
