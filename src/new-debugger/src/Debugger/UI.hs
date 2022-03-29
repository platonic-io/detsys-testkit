{-# LANGUAGE OverloadedStrings #-}
module Debugger.UI where

import Brick
import Brick.BChan
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center, vCenter)
import qualified Brick.Widgets.List as L
import Data.Time.LocalTime (LocalTime, TimeZone, utcToLocalTime)
import Lens.Micro ((^.))
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
           , borderWithLabel (str "Sequence Diagram") $ renderSeqDia as
           ]
         , vLimit 3 $ hBox
           [ borderWithLabel (str "Input") $ renderMessage as
           , hLimit 9 $ borderWithLabel (str "Version") $ renderVersion as
           , borderWithLabel (str "Output") $ renderSentMessage as
           ]
         , vLimit 7 $ borderWithLabel (str "Logs") $ renderLogs as
         ]

renderEvent :: LocalTime -> DebEvent -> String
renderEvent localTime (DebEvent from to event receivedLogical _msg) =
  take 22 (show localTime)  <> ": " <>
  event <> ": " <> from <> " -> " <> to <> " @ " <> show receivedLogical

renderOutputEvent :: DebEvent -> String
renderOutputEvent (DebEvent _ to _ _ msg) =
  to <> ": " <> msg

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
-- only works for greedy sizes
withHeight :: (Int -> Widget n) -> Widget n
withHeight c = Widget
  { hSize = Greedy
  , vSize = Greedy
  , render = do
      ctx <- getContext
      render (c $ ctx ^. availHeightL)
  }

renderSeqDia :: AppState -> Widget ()
renderSeqDia as = withHeight $ \h ->
  fillWidth $ vCenter $ myWrap
  (fromMaybe "?" . fmap (($ h) . isSeqDia . snd) $ L.listSelectedElement $ asLog as)
  where
    fillWidth x = hBox [x, center $ str " "]
    myWrap = vBox . map myStr . lines
    myStr x = hBox [ raw $ V.text' a c
                   | Segment a c <- parseANSI (Text.pack x)
                   ]

renderEvents :: AppState -> Widget ()
renderEvents as =
  center $ L.renderList (listDrawElement $ asTimeZone as) True $ asLog as

renderMessage :: AppState -> Widget ()
renderMessage as = renderToString as (message . isCurrentEvent)

renderVersion :: AppState -> Widget ()
renderVersion as = renderToString as (show . isRunningVersion)

renderSentMessage :: AppState -> Widget ()
renderSentMessage as = renderToString as (addEmpty . unlines . map renderOutputEvent . isSent)
  where
    addEmpty [] = "\n"
    addEmpty xs = xs

renderLogs :: AppState -> Widget ()
renderLogs as = renderToString as (addEmpty . unlines . isLogs)
  where
    addEmpty [] = "\n"
    addEmpty xs = xs

listDrawElement :: TimeZone -> Bool -> InstanceState -> Widget ()
listDrawElement tz sel is =
  let selStr s = if sel
                 then withAttr customAttr (str $ ">" <> s)
                 else str $ " " <> s
  in selStr $ renderEvent (utcToLocalTime tz (isReceivedTime is)) $ isCurrentEvent is

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
    ev -> continue =<< fmap (\xs -> as {asLog = xs}) (L.handleListEventVi L.handleListEvent ev (asLog as))
appEvent as (AppEvent (UpdateState values')) = continue (updateAppState values' as)
appEvent as _ = continue as

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (customAttr, fg V.cyan)
    ]

data AppState = AppState
  { asLog :: L.List () InstanceState
  , asTimeZone :: TimeZone
  }

mkAppState :: TimeZone -> Vector.Vector InstanceState -> AppState
mkAppState tz values = AppState
  { asLog = L.list () values 1
  , asTimeZone = tz
  }

updateAppState :: Vector.Vector InstanceState -> AppState -> AppState
updateAppState values' (AppState l0 tz) = AppState (go (length l0) l0) tz
  where
    go :: Int -> L.List () InstanceState -> L.List () InstanceState
    go n l | n >= Vector.length values' = l
           | otherwise = go (n + 1) (L.listInsert n (values' Vector.! n) l)

runApp :: AppState -> Maybe (BChan AppEvent) -> IO ()
runApp as mBchan = do
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void (customMain initialVty builder mBchan brickApp as)
