{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module IRC.SocketLogger where

import qualified Data.Text as T
import Data.Text.IO.Utf8 (hPutStrLn)
import Relude

data SocketLogger = SocketLogger
  { logOutgoing :: Text -> IO (),
    logIncoming :: [Text] -> IO ()
  }

nullLogger :: SocketLogger
nullLogger = SocketLogger mempty mempty

fileLogger :: Handle -> SocketLogger
fileLogger h = SocketLogger {logOutgoing = logOut, logIncoming = logIn}
  where
    separator = T.replicate 80 "-" <> "\n"
    logOut text = hPutStrLn h $ unlines [separator, "Outgoing:", "", text]
    logIn msgs = hPutStrLn h $ unlines ([separator, "Incoming:", ""] <> msgs)

withSocketLogger :: Maybe FilePath -> (SocketLogger -> IO a) -> IO a
withSocketLogger Nothing f = f nullLogger
withSocketLogger (Just path) f = withFile path AppendMode $ \h -> do
  hSetBuffering h LineBuffering
  f $ fileLogger h
