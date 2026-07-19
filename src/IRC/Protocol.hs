{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Wire-level IRC protocol types and serialisation (RFC 1459 / RFC 2812).
--
-- Types in this module represent raw IRC messages as they travel over the wire,
-- together with encoding and decoding functions.
module IRC.Protocol where

import Data.GenValidity
import Data.GenValidity.Text ()
import Data.List (unsnoc)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Relude hiding (atomically)
import Test.QuickCheck (Gen, choose, elements, oneof)

-- | A raw IRC message as it travels over the wire (RFC 1459 / RFC 2812).
--
-- >>> decodeMessage ":alice!alice@example.com PRIVMSG #haskell :Hello!\r\n"
-- Just
--   (Message
--     (Just (PrefixUser (User (Nickname "alice") (Just (Username "alice"))
--     (Just "example.com")))) PRIVMSG (Params ["#haskell", "Hello!"]))
--
-- >>> decodeMessage "PING :irc.libera.chat\r\n"
-- Just (Message Nothing PING (Params ["irc.libera.chat"]))
data Message = Message
  { prefix :: Maybe Prefix,
    command :: Command,
    params :: Params
  }
  deriving (Show, Eq, Generic)

-- | The origin of a 'Message' — either a 'Server' or a 'User'.
--
-- > :irc.libera.chat         -> 'PrefixServer'
-- > :alice!alice@example.com -> 'PrefixUser'
data Prefix = PrefixServer Server | PrefixUser User
  deriving (Show, Eq, Generic)

-- | IRC protocol command. Most IRC verbs are represented directly;
-- numeric replies (e.g. 001 = welcome, 353 = names list) use 'Numeric'.
--
-- >>> parseCommand "PRIVMSG"
-- Just PRIVMSG
--
-- >>> parseCommand "353"
-- Just (Numeric 353)
data Command
  = NICK
  | USER
  | JOIN
  | PART
  | PRIVMSG
  | NOTICE
  | PING
  | PONG
  | QUIT
  | LIST
  | NAMES
  | AWAY
  | TOPIC
  | Numeric Int
  deriving (Show, Eq, Generic)

-- | The space-separated parameters of an IRC 'Message'.
-- The last parameter may contain spaces when prefixed with @:@ on the wire.
--
-- >>> parseParams "#haskell :Hello, world!"
-- Params ["#haskell","Hello, world!"]
newtype Params = Params {unParams :: [Text]}
  deriving (Show, Eq, Generic)

instance Semigroup Params where (Params a) <> (Params b) = Params $ a <> b

instance Monoid Params where mempty = Params mempty

-- | An IRC server name, e.g. @irc.libera.chat@ or @*.net@.
newtype Server = Server Text
  deriving (Show, Eq, Generic)

-- | An IRC user, identified by 'Nickname', optional 'Username' (ident), and
-- optional hostname. Only 'nickname' is guaranteed to be present.
--
-- > :alice!alice@example.com  ->  User (Nickname "alice")
-- >                                    (Just (Username "alice"))
-- >                                    (Just "example.com")
-- > :bob                      ->  User (Nickname "bob") Nothing Nothing
data User = User
  { nickname :: Nickname,
    username :: Maybe Username,
    host :: Maybe Text
  }
  deriving (Show, Eq, Generic)

-- | A user's nickname, e.g. @alice@, @bob@.
newtype Nickname = Nickname {unNickname :: Text}
  deriving (Show, Eq, Ord, Generic)

-- | A user's ident username (the @user@ in @nick!user\@host@), e.g. @alice@.
newtype Username = Username Text
  deriving (Show, Eq, Generic)

-- | Serialise a 'Message' to wire format (with trailing @\r\n@).
--
-- >>> encodeMessage (Message Nothing PING (Params ["irc.libera.chat"]))
-- "PING :irc.libera.chat\r\n"
encodeMessage :: Message -> Text
encodeMessage (Message pre cmd ps) =
  prefixPart <> commandText <> encodeParams ps <> "\r\n"
  where
    prefixPart = case pre of
      Nothing -> ""
      Just p -> ":" <> encodePrefix p <> " "
    commandText = case cmd of
      Numeric n -> show n
      nonNumeric -> show nonNumeric

-- | Serialise a 'Prefix' to wire format (without the leading @:@).
--
-- >>> encodePrefix (PrefixServer (Server "irc.libera.chat"))
-- "irc.libera.chat"
--
-- >>> encodePrefix
--      $ PrefixUser
--      $ User (Nickname "alice") (Just (Username "alice")) (Just "example.com")
-- "alice!alice@example.com"
encodePrefix :: Prefix -> Text
encodePrefix (PrefixServer (Server name)) = name
encodePrefix (PrefixUser (User (Nickname nick) mUsername mHost)) =
  nick <> userPart <> hostPart
  where
    userPart = case mUsername of
      Nothing -> ""
      Just (Username name) -> "!" <> name
    hostPart = case mHost of
      Nothing -> ""
      Just hostName -> "@" <> hostName

-- | Serialise 'Params' to wire format.
-- The last parameter is encoded as a trailing param (prefixed with @:@)
-- if it contains spaces or starts with @:@.
--
-- >>> encodeParams (Params ["#haskell"])
-- " #haskell"
--
-- >>< encodeParams (Params ["#haskell","Hello, world!"])
-- " #haskell :Hello, world!"
encodeParams :: Params -> Text
encodeParams (Params xs) = case unsnoc xs of
  Nothing -> ""
  Just (initial, lastP) -> " " <> T.unwords (initial <> [encodeTrailing lastP])
  where
    encodeTrailing t
      | T.null t = ":"
      | " " `T.isInfixOf` t = ":" <> t
      | ":" `T.isPrefixOf` t = ":" <> t
      | otherwise = t

-- | Parse a raw IRC message into a structured 'Message'.
-- Wire format:  [:@prefix@ ]@command@ [@param@ ... [:@trailing@]]
--
-- Delegates to 'parsePrefix', 'parseCommand', and 'parseParams' for each
-- section, carrying over the remaining text through after each extraction.
--
-- >>> decodeMessage ":alice!alice@example.com PRIVMSG #haskell :Hello!\r\n"
-- Just (Message (Just (PrefixUser ...)) PRIVMSG (Params ["#haskell","Hello!"]))
--
-- >>> decodeMessage "PING :irc.libera.chat\r\n"
-- Just (Message Nothing PING (Params ["irc.libera.chat"]))
--
-- >>> decodeMessage ""
-- Nothing
decodeMessage :: Text -> Maybe Message
decodeMessage "" = Nothing
decodeMessage txt = do
  (mPrefix, rest) <- parsePrefix txt
  let (cmdStr, rest') = T.breakOn " " rest
  cmd <- parseCommand $ T.stripEnd cmdStr
  let ps = parseParams $ T.stripStart rest'
  Just $ Message mPrefix cmd ps
  where
    -- If the raw line starts with ':', the prefix runs from char 1
    -- up to the first space.  Otherwise there is no prefix.
    parsePrefix t
      | ":" `T.isPrefixOf` t = do
          let (p, r) = T.breakOn " " (T.drop 1 t)
          pref <- decodePrefix p
          Just (Just pref, T.stripStart r)
      | otherwise = Just (Nothing, t)

-- | Match a known verbatim command name, or fall back to a 3-digit
-- numeric reply code (e.g. @001@, @353@).
--
-- >>> parseCommand "PRIVMSG"
-- Just PRIVMSG
--
-- >>> parseCommand "353"
-- Just (Numeric 353)
--
-- >>> parseCommand "BOGUS"
-- Nothing
parseCommand :: Text -> Maybe Command
parseCommand t = case t of
  "NICK" -> Just NICK
  "USER" -> Just USER
  "JOIN" -> Just JOIN
  "PART" -> Just PART
  "PRIVMSG" -> Just PRIVMSG
  "NOTICE" -> Just NOTICE
  "PING" -> Just PING
  "PONG" -> Just PONG
  "QUIT" -> Just QUIT
  "LIST" -> Just LIST
  "NAMES" -> Just NAMES
  "AWAY" -> Just AWAY
  "TOPIC" -> Just TOPIC
  _ -> case TR.decimal t of
    Right (n, _) -> Just (Numeric n)
    _ -> Nothing

-- | Decode a prefix string into a structured 'Prefix'.
--
-- * @server.name@     -> 'PrefixServer'
-- * @nick!user\@host@ -> 'PrefixUser' (all three parts)
-- * @nick\@host@      -> 'PrefixUser' (no username)
--
-- > decodePrefix "irc.libera.chat"
-- Just (PrefixServer (Server "irc.libera.chat"))
--
-- > decodePrefix "alice!alice@example.com"
-- Just (PrefixUser (User (Nickname "alice") ...))
--
-- > decodePrefix "bob@example.com"
-- Just (PrefixUser (User (Nickname "bob") Nothing ...))
decodePrefix :: Text -> Maybe Prefix
decodePrefix t
  | "!" `T.isInfixOf` t =
      -- split on '!' to get nick and the "user@host" portion
      let (nickStr, rest1) = T.breakOn "!" t
          (u, h) = T.breakOn "@" (T.drop 1 rest1)
          (userStr, hostStr) =
            (u, if T.null h then Nothing else Just (T.drop 1 h))
          uname = if T.null userStr then Nothing else Just (Username userStr)
          pref = PrefixUser $ User (Nickname nickStr) uname hostStr
       in Just pref
  | "@" `T.isInfixOf` t =
      -- no '!', so it's nick@host without a username
      let (nickStr, hostStr) = T.breakOn "@" t
          user = User (Nickname nickStr) Nothing $ Just (T.drop 1 hostStr)
       in Just $ PrefixUser user
  | otherwise = Just $ PrefixServer (Server t)

-- | Split the parameter portion of an IRC message into a list of
-- individual parameters.
-- The first token prefixed with @:@ (and everything after it) is treated
-- as a single trailing parameter; the colon is stripped and the rest is kept
-- (including spaces). All tokens before it are middle parameters.
--
-- >>> parseParams "#haskell :Hello, world!"
-- Params ["#haskell","Hello, world!"]
--
-- >>> parseParams ""
-- >   -- Params []
parseParams :: Text -> Params
parseParams t = Params $ case break (":" `T.isPrefixOf`) (T.words t) of
  (middle, []) -> middle
  (middle, trailing : rest) ->
    middle
      <> [T.drop 1 trailing <> if null rest then "" else " " <> T.unwords rest]

--------------------------------------------------------------------------------

instance Validity Params where
  validate (Params []) = mempty
  validate (Params ps) = middleV <> lastV <> allV
    where
      allV = mconcat $ do
        p <- ps
        [checkNoLinebreak p "Param contains carriage return"]
      dropLast = reverse . drop 1 . reverse
      middleV = mconcat $ do
        x <- dropLast ps
        [ check (not (":" `T.isPrefixOf` x)) "Middle param starts with ':'",
          checkNoSpaces x "Middle param contains whitespace",
          checkNotNull x "Middle param is empty"
          ]
      lastV = case ps of
        [x] -> check (not (T.all isSpace x)) "Trailing param is all-whitespace"
        _ -> mempty

instance GenValid Params where
  genValid = do
    useTrailing <- choose (0, 2) :: Gen Int
    case useTrailing of
      0 -> genWithTrailing
      _ -> genMiddleOnly
    where
      alphanumeric = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']
      safeChar = elements $ alphanumeric <> "!#$%&'*+-./<=>?@\\^_`|~"
      safeTrailingChar =
        elements $ alphanumeric <> " !#$%&'*+,-./:;<=>?@\\^_`|~"
      genMiddleOnly = do
        n <- choose (0, 5)
        Params <$> replicateM n (genFromChars safeChar 1 10)
      genWithTrailing = do
        n <- choose (0, 3)
        ps <- replicateM n (genFromChars safeChar 1 10)
        trailing <- T.stripEnd <$> genFromChars safeTrailingChar 0 15
        pure $ Params $ ps <> [trailing]
      genFromChars chars minLen maxLen = do
        len <- choose (minLen, maxLen)
        T.pack <$> replicateM len chars

instance Validity Nickname where
  validate (Nickname t) =
    mconcat
      [ checkNotNull t "Nickname is empty",
        checkNoLinebreak t "Nickname contains control chars"
      ]

instance GenValid Nickname

instance Validity Username where
  validate (Username t) =
    mconcat
      [ checkNotNull t "Username is empty",
        checkNoLinebreak t "Username contains control chars"
      ]

instance GenValid Username

instance Validity Server where
  validate (Server t) =
    mconcat
      [ checkNotNull t "Server name is empty",
        check (not ("!" `T.isInfixOf` t)) "Server name contains '!'",
        check (not ("@" `T.isInfixOf` t)) "Server name contains '@'",
        checkNoLinebreak t "Server name contains control chars"
      ]

instance GenValid Server

instance Validity User

instance GenValid User where
  genValid = do
    nick <- genValid
    oneof
      [ User nick Nothing . Just <$> genValid,
        User nick . Just <$> genValid <*> pure Nothing,
        User nick . Just <$> genValid <*> (Just <$> genValid)
      ]

instance Validity Prefix

instance GenValid Prefix

instance Validity Message

instance GenValid Message

instance Validity Command where
  validate (Numeric n) = check (n >= 0) "Numeric command is negative"
  validate _ = mempty

instance GenValid Command

checkNotNull :: Text -> String -> Validation
checkNotNull t = check (not (T.null t))

checkNoLinebreak :: Text -> String -> Validation
checkNoLinebreak t = check $ not $ "\r" `T.isInfixOf` t || "\n" `T.isInfixOf` t

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

checkNoSpaces :: Text -> String -> Validation
checkNoSpaces t = check $ not $ T.any isSpace t
