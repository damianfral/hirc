{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module IRC.CLI where

import Data.Version (showVersion)
import IRC.Client
import IRC.Domain
import IRC.Protocol
import IRC.UI (runUI)
import Options.Generic
import Paths_hirc (version)
import Relude

data Options w = Options
  { nickname :: w ::: Text,
    username :: w ::: Text,
    realname :: w ::: Text,
    host :: w ::: String <!> "irc.libera.chat" <?> "IRC server hostname",
    port :: w ::: String <!> "6667" <?> "IRC server port",
    logFile :: w ::: Maybe FilePath <?> "Write logs to this file"
  }
  deriving (Generic)

instance ParseRecord (Options Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

runCLI :: IO ()
runCLI = do
  unwrapRecord msg >>= runCLIOptions
  exitSuccess
  where
    msg = unwords ["hirc", "v" <> show (showVersion version)]

runCLIOptions :: Options Unwrapped -> IO ()
runCLIOptions Options {..} =
  withIRCClient (IRCClientSettings host port logFile) $ \client -> do
    writeAction client
      $ Register (Nickname nickname) (Username username) (Realname realname)
    runUI host client $ User (Nickname nickname) Nothing Nothing
