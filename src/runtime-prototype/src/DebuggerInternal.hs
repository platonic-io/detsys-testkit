{-# LANGUAGE OverloadedStrings #-}

module DebuggerInternal where

import Brick
import Brick.Widgets.Border (borderWithLabel, vBorder, hBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center, hCenter)
import qualified Brick.Widgets.List as L
import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vector
import qualified Graphics.Vty as V
import System.FilePath
import System.IO
import System.Posix.Files
import Text.Wrap
import Network.Socket
import qualified Network.Socket.ByteString.Lazy as Socket

import StuntDouble
import qualified StuntDouble.Transport.UnixSocket as US
import Scheduler.Executor (executorCodec)

import qualified Debugger

------------------------------------------------------------------------

drawUI :: AppState -> [Widget ()]
drawUI as = [ui]
  where
    ui = withBorderStyle unicode
       -- $ borderWithLabel (str "Debugger")
       $ vBox
         [ hBox
           [ vBox
             [ borderWithLabel (str "Reactor State") $ renderReactorState as
             , vLimitPercent 33 $ borderWithLabel (str "Events") $ renderEvents as
             ]
           , borderWithLabel (str "Sequence Diagram") $ renderSeqDia as
           ]
         , vLimit 7 $ hBox
           [ borderWithLabel (str "Current Message") $ renderMessage as
           , borderWithLabel (str "Sent Messages") $ renderSentMessage as
           ]
         , vLimit 7 $ borderWithLabel (str "Reactor Log") $ renderLogs as
         ]

renderEvent :: DebEvent -> String
renderEvent (DebEvent from to event receivedLogical _) =
  event <> ": " <> from <> " -> " <> to <> " @ " <> show receivedLogical

renderToString :: AppState -> (InstanceState -> String) -> Widget ()
renderToString as f = center$ strWrapWith wrapSettings (fromMaybe "?" . fmap (f . snd) $ L.listSelectedElement $ asLog as)

wrapSettings = defaultWrapSettings
  { preserveIndentation = False, breakLongWords = True }

renderReactorState :: AppState -> Widget ()
renderReactorState as = renderToString as state

renderEvents :: AppState -> Widget ()
renderEvents as =
  center $ L.renderList listDrawElement True $ asLog as

renderSeqDia :: AppState -> Widget ()
renderSeqDia as = renderToString as seqDia

renderMessage :: AppState -> Widget ()
renderMessage as = renderToString as (message . currentEvent)

renderSentMessage :: AppState -> Widget ()
renderSentMessage as = renderToString as (addEmpty . unlines . map renderEvent . sent)
  where
    addEmpty [] = "\n"
    addEmpty xs = xs

renderLogs :: AppState -> Widget ()
renderLogs as = renderToString as (addEmpty . unlines . logs)
  where
    addEmpty [] = "\n"
    addEmpty xs = xs

listDrawElement :: Bool -> InstanceState -> Widget ()
listDrawElement sel is =
  let selStr s = if sel
                 then withAttr customAttr (str $ ">" <> s)
                 else str $ " " <> s
  in selStr $ renderEvent $ currentEvent is

customAttr :: AttrName
customAttr = L.listSelectedAttr <> "custom"

brickApp :: App AppState e ()
brickApp = App
  { appDraw = drawUI
  , appHandleEvent = appEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  , appChooseCursor = neverShowCursor
  }

appEvent :: AppState -> BrickEvent () e -> EventM () (Next AppState)
appEvent as (VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'q') [] -> halt as
    ev -> continue =<< fmap AppState (L.handleListEventVi L.handleListEvent ev (asLog as))
appEvent as _ = continue as

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (customAttr, fg V.cyan)
    ]

data DebEvent = DebEvent
  { from :: String
  , to :: String
  , event :: String
  , receivedLogical :: Int
  -- , receivedSimulated :: Time
  , message :: String
  } deriving Show

data InstanceState = InstanceState
 { state :: String -- Should probably be per reactor
 , currentEvent :: DebEvent
 , seqDia :: String
 , logs :: [String]
 , sent :: [DebEvent]
 }

data AppState = AppState
  { asLog :: L.List () InstanceState
  }

fakeState :: AppState
fakeState = AppState
  { asLog = L.list () (Vector.fromList [f1, f2,f1,f1,f2,f2,f3,f1,f1,f2,f2,f2,f1,f1,f1,f2]) 1
  }
  where
    e1 = DebEvent
      { from = "A"
      , to = "B"
      , event = "fst"
      , receivedLogical = 0
      , message = "This is the first message"
      }
    e2 = DebEvent
      { from = "B"
      , to = "A"
      , event = "snd"
      , receivedLogical = 1
      , message = "This is the second message"
      }
    f1 = InstanceState
      { state = "state of A"
      , currentEvent = e1
      , seqDia = "sequence diagram from 0"
      , logs = ["first log of A", "second log of A"]
      , sent = [e2]
      }
    f2 = InstanceState
      { state = "state of B"
      , currentEvent = e2
      , seqDia = "sequence diagram from 1"
      , logs = ["Log for B"]
      , sent = []
      }
    f3 = InstanceState
      { state = "state of B"
      , currentEvent = e2
      , seqDia = "sequence diagram from 1"
      , logs = ["Log for B"]
      , sent = [e1,e2,e1,e1,e1]
      }

main :: IO ()
main = do
  void (defaultMain brickApp fakeState)
