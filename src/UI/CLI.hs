{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.CLI where

import Data.Version (showVersion)
import IRC.Client
import IRC.Domain
import IRC.Protocol
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
    port :: ServiceName,
    logFile :: Maybe FilePath
  }

nicknameParser :: Parser Nickname
nicknameParser = strOption mods <&> Nickname . fromString
  where
    mods = long "nickname" <> help "Nickname" <> metavar "TEXT"

usernameParser :: Parser Username
usernameParser = strOption mods <&> Username . fromString
  where
    mods = long "username" <> help "Username" <> metavar "TEXT"

realnameParser :: Parser Realname
realnameParser = strOption mods <&> Realname . fromString
  where
    mods = long "realname" <> help "Realname" <> metavar "TEXT"

hostParser :: Parser HostName
hostParser =
  strOption
    $ long "host"
    <> help "IRC server hostname"
    <> metavar "HOSTNAME"
    <> value "irc.libera.chat"
    <> showDefault

portParser :: Parser ServiceName
portParser =
  strOption
    $ long "port"
    <> help "IRC server port"
    <> metavar "PORT"
    <> value "6667"
    <> showDefault

logFileParser :: Parser (Maybe FilePath)
logFileParser = optional $ strOption mods
  where
    mods = long "log-file" <> help "Write logs to this file" <> metavar "FILE"

optionsParser :: Parser Options
optionsParser =
  Options
    <$> nicknameParser
    <*> usernameParser
    <*> realnameParser
    <*> hostParser
    <*> portParser
    <*> logFileParser

runCLI :: IO ()
runCLI = execParser opts >>= runCLIOptions
  where
    opts = info (optionsParser <**> helper) infoMod
    infoMod =
      fullDesc
        <> header ("hirc v" <> show (showVersion version))
        <> progDesc "Haskell IRC client"

runCLIOptions :: Options -> IO ()
runCLIOptions (Options nick user real hostname p logPath) =
  withIRCClient (IRCClientSettings hostname p logPath) $ \client -> do
    writeAction client $ Register nick user real
    runUI hostname client $ User nick Nothing Nothing
