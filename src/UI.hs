{-# LANGUAGE NoImplicitPrelude #-}

module UI (runUI) where

import Brick
import qualified Brick.AttrMap as A
import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent.Async (async, cancel)
import qualified Data.Map as Map
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import IRC.Client
import IRC.Protocol (Server (..), User (..))
import Network.Socket (HostName)
import Relude
import UI.AppState
import UI.Chat (Chat (Chat), ChatID (ChatWithServer))
import UI.EventHandler (handleEvent)
import UI.Style (attributes)
import UI.View

uiApp :: App AppState Event ViewportName
uiApp =
  App
    { appDraw = viewUI,
      appChooseCursor = const $ showCursorNamed Input,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const $ A.attrMap V.defAttr attributes
    }

runUI :: HostName -> IRCClient -> User -> IO ()
runUI hostname client user = do
  vty <- buildVty
  bchan <- newBChan 256
  bchanLoopAsync <- liftIO $ async $ ircClientToBChanEventLoop client bchan
  void $ customMain vty buildVty (Just bchan) uiApp initialState
  cancel bchanLoopAsync
  where
    buildVty = do
      v <- mkVty V.defaultConfig
      V.setMode (V.outputIface v) V.Mouse True -- mouse support
      pure v
    server = Server $ fromString hostname
    initialState =
      AppState
        { appClient = client,
          appUser = user,
          appCurrentChat = ChatWithServer server,
          appChats = Map.singleton (ChatWithServer server) (Chat mempty mempty 0),
          appServer = server,
          appInput = emptyEditor
        }

--------------------------------------------------------------------------------

ircClientToBChanEventLoop :: IRCClient -> BChan Event -> IO ()
ircClientToBChanEventLoop client bchan = loop
  where
    loop = do
      event <- readEvent client
      writeBChan bchan event
      case event of
        Disconnected _ -> pure ()
        _ -> loop
