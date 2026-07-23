module UI.Chat where

import qualified Data.Set as Set
import Data.Time.Clock (UTCTime)
import IRC.Domain
import IRC.Protocol
import Relude

data ChatID
  = ChatWithServer Server
  | ChatWithChannel Channel
  | ChatWithNickname Nickname
  deriving (Eq, Ord)

data Chat = Chat
  { chatMessages :: [ChatMessage],
    chatMembers :: Set Nickname,
    chatUnreadCount :: Int
  }
  deriving (Eq)

instance Semigroup Chat where
  Chat msgs1 members1 unread1 <> Chat msgs2 members2 unread2 =
    Chat (msgs1 <> msgs2) (members1 <> members2) (unread1 + unread2)

instance Monoid Chat where mempty = Chat mempty mempty 0

data ChatMessage = ChatMessage
  { chatMessageTime :: UTCTime,
    -- | Nothing for nmessages comming from server
    chatMessageFrom :: Maybe Nickname,
    chatMessageContent :: [Text],
    chatMessageTag :: Tag
  }
  deriving (Eq)

data Tag = Normal | Notice | CommandReply | ServerEvent | Info
  deriving (Show, Eq)

newChat :: Set Nickname -> Chat
newChat members = mempty {chatMembers = members}

readChat :: Chat -> Chat
readChat c = c {chatUnreadCount = 0}

appendChatMessage :: ChatMessage -> Chat -> Chat
appendChatMessage msg chat =
  chat
    { chatMessages = chatMessages chat <> [msg],
      chatUnreadCount = chatUnreadCount chat + 1
    }

appendChatMessages :: [ChatMessage] -> Chat -> Chat
appendChatMessages msgs chat = foldl' (flip appendChatMessage) chat msgs

addNickname :: Nickname -> Chat -> Chat
addNickname nick chat = chat {chatMembers = chatMembers chat & Set.insert nick}

addNicknames :: [Nickname] -> Chat -> Chat
addNicknames msgs chat = foldl' (flip addNickname) chat msgs

removeNickname :: Nickname -> Chat -> Chat
removeNickname nick chat = chat {chatMembers = Set.delete nick $ chatMembers chat}
