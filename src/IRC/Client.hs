{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module IRC.Client
  ( writeAction,
    readEvent,
    withIRCClient,
    IRCClientSettings (..),
    IRCClient,
    Action (..),
    Event (..),
  )
where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (IOException, bracket, throwIO, try)
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import IRC.Domain
import IRC.Protocol
import IRC.SocketLogger
import Network.Socket (AddrInfo (..), HostName, ServiceName, Socket, SocketType (..), close, connect, defaultHints, getAddrInfo, openSocket)
import Network.Socket.ByteString
import Relude hiding (atomically)
import System.IO.Error (userError)

data IRCClient = IRCClient {actions :: TBQueue Action, events :: TBQueue Event}

data IRCState = IRCState
  { socket :: Socket,
    client :: IRCClient,
    connectionThread :: Async ()
  }

writeAction :: IRCClient -> Action -> IO ()
writeAction IRCClient {actions} = atomically . writeTBQueue actions

readAction :: IRCClient -> IO Action
readAction IRCClient {actions} = atomically $ readTBQueue actions

writeEvent :: IRCClient -> Event -> IO ()
writeEvent IRCClient {events} = atomically . writeTBQueue events

readEvent :: IRCClient -> IO Event
readEvent IRCClient {events} = atomically $ readTBQueue events

initIRCState :: Socket -> SocketLogger -> IRCClient -> IO IRCState
initIRCState socket logger client = do
  thread <- async $ do
    result <- try $ concurrently_ sendLoop (receiveLoop "")
    case result of
      Left err -> writeEvent client $ Disconnected $ show (err :: IOException)
      Right _ -> pure ()
  pure $ IRCState socket client thread
  where
    sendMessage message = do
      let encodedMessage = encodeMessage message
      sendAll socket $ encodeUtf8 encodedMessage
      logOutgoing logger encodedMessage

    sendLoop = forever $ do
      action <- readAction client
      let messages = actionToMessages action
      forM_ messages sendMessage
    receiveLoop acc = do
      bytes <- recv socket 4096
      -- 0 bytes means the server has closed the connection (FIN).
      if BS.null bytes
        then throwIO $ userError "connection closed by server" -- IOException
        else do
          let text = acc <> decodeUtf8With lenientDecode bytes
          let (raw, remaining) = T.breakOnEnd "\r\n" text
          let rawMessages = T.splitOn "\r\n" raw
          logIncoming logger rawMessages
          forM_ rawMessages $ \msg -> do
            case decodeMessage msg of
              Nothing -> pure ()
              Just (Message _ PING params) ->
                sendMessage $ Message Nothing PONG params
              Just message -> for_ (messageToEvent message) (writeEvent client)
          receiveLoop remaining

data IRCClientSettings = IRCClientSettings HostName ServiceName (Maybe FilePath)

withIRCClient :: IRCClientSettings -> (IRCClient -> IO a) -> IO a
withIRCClient (IRCClientSettings hostname port mLogFile) run =
  withSocketLogger mLogFile $ \logger ->
    bracket (acquireSocket logger) releaseSocket $ run . client
  where
    hints = defaultHints {addrSocketType = Stream}
    acquireSocket logger = do
      addr <- NE.head <$> getAddrInfo (Just hints) (Just hostname) (Just port)
      socket <- openSocket addr
      connect socket $ addrAddress addr
      client <- IRCClient <$> newTBQueueIO 32 <*> newTBQueueIO 32
      initIRCState socket logger client
    releaseSocket (IRCState {connectionThread, socket}) = do
      cancel connectionThread
      close socket
