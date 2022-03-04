{-# LANGUAGE OverloadedStrings #-}
module Debugger.UI where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import qualified Brick.Widgets.List as L
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vector
import qualified Graphics.Vty as V
import Text.Wrap

import Debugger.State

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
renderReactorState as = renderToString as isState

renderEvents :: AppState -> Widget ()
renderEvents as =
  center $ L.renderList listDrawElement True $ asLog as

renderSeqDia :: AppState -> Widget ()
renderSeqDia as = renderToString as isSeqDia

renderMessage :: AppState -> Widget ()
renderMessage as = renderToString as (message . isCurrentEvent)

renderSentMessage :: AppState -> Widget ()
renderSentMessage as = renderToString as (addEmpty . unlines . map renderEvent . isSent)
  where
    addEmpty [] = "\n"
    addEmpty xs = xs

renderLogs :: AppState -> Widget ()
renderLogs as = renderToString as (addEmpty . unlines . isLogs)
  where
    addEmpty [] = "\n"
    addEmpty xs = xs

listDrawElement :: Bool -> InstanceState -> Widget ()
listDrawElement sel is =
  let selStr s = if sel
                 then withAttr customAttr (str $ ">" <> s)
                 else str $ " " <> s
  in selStr $ renderEvent $ isCurrentEvent is

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

data AppState = AppState
  { asLog :: L.List () InstanceState
  }

mkAppState :: Vector.Vector InstanceState -> AppState
mkAppState values = AppState
  { asLog = L.list () values 1
  }

runApp :: AppState -> IO ()
runApp as = void (defaultMain brickApp as)
