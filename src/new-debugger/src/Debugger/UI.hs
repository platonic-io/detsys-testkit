{-# LANGUAGE OverloadedStrings #-}
module Debugger.UI where

import Brick
import Brick.BChan
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center, vCenter)
import qualified Brick.Widgets.List as L
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Graphics.Vty as V
import Text.Wrap

import Debugger.AnsiEscape (Segment(..), parseANSI)
import Debugger.State hiding (to, from, event, receivedLogical)

------------------------------------------------------------------------

drawUI :: AppState -> [Widget ()]
drawUI as = [ui]
  where
    ui = withBorderStyle unicode
       -- $ borderWithLabel (str "Debugger")
       $ vBox
         [ hBox
           [ vBox
             [ borderWithLabel (str "State") $ renderReactorState as
             , vLimitPercent 33 $ borderWithLabel (str "Events") $ renderEvents as
             ]
           , vBox
             [ borderWithLabel (str "Input") $ renderMessage as
             , vLimit 3 $ borderWithLabel (str "Version") $ renderVersion as
             , borderWithLabel (str "Output") $ renderSentMessage as
             ]
           ]
         , vLimit 7 $ borderWithLabel (str "Logs") $ renderLogs as
         ]

renderEvent :: Bool -> DebEvent -> String
renderEvent showMsg (DebEvent from to event receivedLogical msg) =
  event <> ": " <> from <> " -> " <> to <> " @ " <> show receivedLogical
        <> if showMsg then " : " <> msg else mempty

renderToString :: AppState -> (InstanceState -> String) -> Widget ()
renderToString as f = center $ strWrapWith wrapSettings
  (fromMaybe "?" . fmap (f . snd) $ L.listSelectedElement $ asLog as)

wrapSettings :: WrapSettings
wrapSettings = defaultWrapSettings
  { preserveIndentation = False, breakLongWords = True }

renderReactorState :: AppState -> Widget ()
renderReactorState as = fillWidth $ vCenter $ myWrap
  (fromMaybe "?" . fmap (isState . snd) $ L.listSelectedElement $ asLog as)
  where
    fillWidth x = hBox [x, center $ str " "]
    myWrap = vBox . map myStr . lines
    myStr x = hBox [ raw $ V.text' a c
                   | Segment a c <- parseANSI (Text.pack x)
                   ]

renderEvents :: AppState -> Widget ()
renderEvents as =
  center $ L.renderList listDrawElement True $ asLog as

renderSeqDia :: AppState -> Widget ()
renderSeqDia as = renderToString as isSeqDia

renderMessage :: AppState -> Widget ()
renderMessage as = renderToString as (message . isCurrentEvent)

renderVersion :: AppState -> Widget ()
renderVersion as = renderToString as (show . isRunningVersion)

renderSentMessage :: AppState -> Widget ()
renderSentMessage as = renderToString as (addEmpty . unlines . map (renderEvent True) . isSent)
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
  in selStr $ renderEvent False $ isCurrentEvent is

customAttr :: AttrName
customAttr = L.listSelectedAttr <> "custom"

brickApp :: App AppState AppEvent ()
brickApp = App
  { appDraw = drawUI
  , appHandleEvent = appEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  , appChooseCursor = neverShowCursor
  }

data AppEvent = UpdateState (Vector.Vector InstanceState)

appEvent :: AppState -> BrickEvent () AppEvent -> EventM () (Next AppState)
appEvent as (VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'q') [] -> halt as
    ev -> continue =<< fmap AppState (L.handleListEventVi L.handleListEvent ev (asLog as))
appEvent as (AppEvent (UpdateState values')) = continue (updateAppState values' as)
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

updateAppState :: Vector.Vector InstanceState -> AppState -> AppState
updateAppState values' (AppState l0) = AppState (go (length l0) l0)
  where
    go :: Int -> L.List () InstanceState -> L.List () InstanceState
    go n l | n >= Vector.length values' = l
           | otherwise = go (n + 1) (L.listInsert n (values' Vector.! n) l)

runApp :: AppState -> Maybe (BChan AppEvent) -> IO ()
runApp as mBchan = do
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void (customMain initialVty builder mBchan brickApp as)
