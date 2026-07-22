{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module IRC.Transport where

import Control.Exception (throwIO, try)
import qualified Data.ByteString as BS
import Network.Socket (HostName, Socket, close)
import Network.Socket.ByteString (recv, sendAll)
import Network.TLS (ClientHooks (..), ClientParams (..), bye, contextClose, contextNew, defaultClientHooks, defaultParamsClient, handshake, recvData, sendData)
import Relude
import System.IO (hPutStrLn)

data Transport = Transport
  { transportSend :: ByteString -> IO (),
    transportRecv :: Int -> IO ByteString,
    transportClose :: IO ()
  }

makePlainTransport :: Socket -> Transport
makePlainTransport sock =
  Transport
    { transportSend = sendAll sock,
      transportRecv = recv sock,
      transportClose = close sock
    }

makeTLSTransport :: Socket -> HostName -> IO Transport
makeTLSTransport sock hostname = do
  let hooks = defaultClientHooks {onServerCertificate = \_ _ _ _ -> pure []}
      ps = (defaultParamsClient hostname "") {clientHooks = hooks}
  result <- try $ do
    ctx <- contextNew sock ps
    handshake ctx
    pure ctx
  case result of
    Right ctx ->
      pure
        Transport
          { transportSend = sendData ctx . BS.fromStrict,
            transportRecv = const $ recvData ctx,
            transportClose = bye ctx >> contextClose ctx
          }
    Left ex -> do
      close sock
      hPutStrLn stderr $ "Cannot open TLS connection: " <> show ex
      throwIO (ex :: SomeException)
