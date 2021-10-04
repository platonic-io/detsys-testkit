{-# LANGUAGE OverloadedStrings #-}

module Debugger where

import Brick
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center, hCenter)
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vector
import Control.Concurrent.Async
import Control.Monad
import qualified Graphics.Vty as V
import System.FilePath
import System.IO
import System.Posix.Files

import StuntDouble

------------------------------------------------------------------------

readLog :: IO Log
readLog = do
  let pipe = "/tmp" </> "scheduler-admin"
  -- NOTE: We need to start reading the response before making the request to
  -- dump the log, otherwise the response will be written to the void.
  a <- async (withFile (pipe <> "-response") ReadWriteMode hGetLine)
  appendFile pipe "AdminDumpLog\n"
  s <- wait a
  return (read s)

drawUI :: AppState -> [Widget ()]
drawUI (AppState l) = [ui]
  where
    ui = withBorderStyle unicode
       $ borderWithLabel (str "Debugger")
       $ (center (L.renderList listDrawElement True l) <+> vBorder <+> center (str "Right"))

listDrawElement :: Bool -> Timestamped LogEntry -> Widget ()
listDrawElement sel (Timestamped le lt pt) =
  let selStr s = if sel
                 then withAttr customAttr (str $ "<" <> s <> ">")
                 else str s
  in hCenter $ selStr $ show le ++ " " ++ show lt ++ " " ++ show pt

customAttr :: AttrName
customAttr = L.listSelectedAttr <> "custom"

app :: App AppState e ()
app = App
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
  { asLog :: L.List () (Timestamped LogEntry)
  }

initialState :: Log -> AppState
initialState (Log es) = AppState
  { asLog = L.list () (Vector.fromList es) 1
  }

main :: IO ()
main = do
  l <- readLog
  void (defaultMain app (initialState l))
