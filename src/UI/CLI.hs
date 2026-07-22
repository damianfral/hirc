{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.CLI where

import Data.Version (showVersion)
import IRC.Client
import IRC.Domain (Realname (..))
import IRC.Protocol (Nickname (..), User (..), Username (..))
import Network.Socket (HostName, ServiceName)
import Options.Applicative
import Paths_hirc (version)
import Relude
import UI (runUI)

data Options = Options
  { nickname :: Nickname,
    username :: Username,
    realname :: Realname,
    host :: HostName,
    port :: Maybe ServiceName,
    noTls :: ConnectionMode,
    logFile :: Maybe FilePath
  }

textOpt :: String -> String -> Parser Text
textOpt l h = strOption $ long l <> help h <> metavar "TEXT"

nicknameParser :: Parser Nickname
nicknameParser = Nickname <$> textOpt "nickname" "Nickname"

usernameParser :: Parser Username
usernameParser = Username <$> textOpt "username" "Username"

realnameParser :: Parser Realname
realnameParser = Realname <$> textOpt "realname" "Realname"

hostParser :: Parser HostName
hostParser =
  strOption
    $ long "host"
    <> help "IRC server hostname"
    <> metavar "HOSTNAME"
    <> value "irc.libera.chat"
    <> showDefault

portParser :: Parser (Maybe ServiceName)
portParser =
  optional
    $ strOption
    $ long "port"
    <> help "IRC server port"
    <> metavar "PORT"
    <> showDefault

connectionModeParser :: Parser ConnectionMode
connectionModeParser =
  fromMaybe TLS <$> optional (flag' Plaintext $ long "no-tls")

logFileParser :: Parser (Maybe FilePath)
logFileParser = optional $ strOption $ do
  long "log-file" <> help "Write logs to this file" <> metavar "FILE"

optionsParser :: Parser Options
optionsParser =
  Options
    <$> nicknameParser
    <*> usernameParser
    <*> realnameParser
    <*> hostParser
    <*> portParser
    <*> connectionModeParser
    <*> logFileParser

runCLI :: IO ()
runCLI = execParser opts >>= runCLIOptions
  where
    opts = info (optionsParser <**> helper) infoMod
    infoMod =
      fullDesc
        <> header ("hirc v" <> showVersion version)
        <> progDesc "Haskell IRC client"

runCLIOptions :: Options -> IO ()
runCLIOptions (Options nick user real hostname p connectionMode logPath) =
  withIRCClient (IRCClientSettings hostname port' connectionMode logPath)
    $ \client -> do
      writeAction client $ Register nick user real
      runUI hostname client $ User nick Nothing Nothing
  where
    connPort = case connectionMode of
      Plaintext -> "6667"
      TLS -> "6697"
    port' = fromMaybe connPort p
