{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module IRC.Client
  ( writeAction,
    readEvent,
    withIRCClient,
    IRCClientSettings (..),
    IRCClient,
    ConnectionMode (..),
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
import IRC.Transport (Transport (..), makePlainTransport, makeTLSTransport)
import Network.Socket (AddrInfo (..), HostName, ServiceName, SocketType (..), connect, defaultHints, getAddrInfo, openSocket)
import Relude hiding (atomically)
import System.IO.Error (userError)

data IRCClient = IRCClient {actions :: TBQueue Action, events :: TBQueue Event}

data IRCState = IRCState
  { transport :: Transport,
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

initIRCState :: Transport -> SocketLogger -> IRCClient -> IO IRCState
initIRCState tr logger cli = do
  thread <- async $ do
    result <- try $ concurrently_ sendLoop (receiveLoop "")
    case result of
      Left err -> writeEvent cli $ Disconnected $ show (err :: IOException)
      Right _ -> pure ()
  pure $ IRCState tr cli thread
  where
    sendMessage message = do
      let encodedMessage = encodeMessage message
      transportSend tr $ encodeUtf8 encodedMessage
      logOutgoing logger encodedMessage

    sendLoop = forever $ do
      action <- readAction cli
      let messages = actionToMessages action
      forM_ messages sendMessage
    receiveLoop acc = do
      bytes <- transportRecv tr 4096
      if BS.null bytes
        then throwIO $ userError "connection closed by server"
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

data ConnectionMode = TLS | Plaintext

data IRCClientSettings
  = IRCClientSettings HostName ServiceName ConnectionMode (Maybe FilePath)

withIRCClient :: IRCClientSettings -> (IRCClient -> IO a) -> IO a
withIRCClient (IRCClientSettings hostname port connectionMode mLogFile) run =
  withSocketLogger mLogFile $ \logger ->
    bracket (acquireConnection logger) releaseConnection $ run . client
  where
    hints = defaultHints {addrSocketType = Stream}
    acquireConnection logger = do
      addr <- NE.head <$> getAddrInfo (Just hints) (Just hostname) (Just port)
      sock <- openSocket addr
      connect sock $ addrAddress addr
      transport' <- case connectionMode of
        TLS -> makeTLSTransport sock hostname
        Plaintext -> pure $ makePlainTransport sock
      cli <- IRCClient <$> newTBQueueIO 32 <*> newTBQueueIO 32
      initIRCState transport' logger cli
    releaseConnection IRCState {connectionThread = t, transport = tr} = do
      cancel t
      transportClose tr
