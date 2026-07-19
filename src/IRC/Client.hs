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
writeAction IRCClient {actions = acts} = atomically . writeTBQueue acts

readAction :: IRCClient -> IO Action
readAction IRCClient {actions = acts} = atomically $ readTBQueue acts

writeEvent :: IRCClient -> Event -> IO ()
writeEvent IRCClient {events = evts} = atomically . writeTBQueue evts

readEvent :: IRCClient -> IO Event
readEvent IRCClient {events = evts} = atomically $ readTBQueue evts

initIRCState :: Socket -> SocketLogger -> IRCClient -> IO IRCState
initIRCState sock logger cli = do
  thread <- async $ do
    result <- try $ concurrently_ sendLoop (receiveLoop "")
    case result of
      Left err -> writeEvent cli $ Disconnected $ show (err :: IOException)
      Right _ -> pure ()
  pure $ IRCState sock cli thread
  where
    sendMessage message = do
      let encodedMessage = encodeMessage message
      sendAll sock $ encodeUtf8 encodedMessage
      logOutgoing logger encodedMessage

    sendLoop = forever $ do
      action <- readAction cli
      let messages = actionToMessages action
      forM_ messages sendMessage
    receiveLoop acc = do
      bytes <- recv sock 4096
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
              Just (Message _ PING ps) ->
                sendMessage $ Message Nothing PONG ps
              Just message -> for_ (messageToEvent message) (writeEvent cli)
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
      sock <- openSocket addr
      connect sock $ addrAddress addr
      cli <- IRCClient <$> newTBQueueIO 32 <*> newTBQueueIO 32
      initIRCState sock logger cli
    releaseSocket IRCState {connectionThread = t, socket = s} = do
      cancel t
      close s
