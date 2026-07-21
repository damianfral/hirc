{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.KeyEvent where

import qualified Brick.Keybindings.KeyConfig as KC
import qualified Brick.Keybindings.KeyEvents as KE
import qualified Graphics.Vty as V
import Relude

data KeyEvent
  = EvQuit
  | EvScrollUp
  | EvScrollDown
  | EvNextChannel
  | EvPrevChannel
  | EvActivate
  deriving (Show, Ord, Eq)

myKeyEvents :: KE.KeyEvents KeyEvent
myKeyEvents =
  KE.keyEvents
    [ ("quit", EvQuit),
      ("scroll-up", EvScrollUp),
      ("scroll-down", EvScrollDown),
      ("next-channel", EvNextChannel),
      ("prev-channel", EvPrevChannel),
      ("activate", EvActivate)
    ]

defaultBindings :: [(KeyEvent, [KC.Binding])]
defaultBindings =
  [ (EvQuit, [KC.bind V.KEsc, KC.ctrl 'c']),
    (EvScrollUp, [KC.bind V.KPageUp]),
    (EvScrollDown, [KC.bind V.KPageDown]),
    (EvNextChannel, [KC.ctrl 'n', KC.ctrl 'j']),
    (EvPrevChannel, [KC.ctrl 'p', KC.ctrl 'k']),
    (EvActivate, [KC.bind V.KEnter])
  ]

keyConfig :: KC.KeyConfig KeyEvent
keyConfig = KC.newKeyConfig myKeyEvents defaultBindings []
