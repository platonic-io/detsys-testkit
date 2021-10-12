{-# LANGUAGE OverloadedStrings #-}

module Debugger where

import Brick
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center, hCenter)
import qualified Brick.Widgets.List as L
import Control.Concurrent.Async
import Control.Monad
import qualified Data.Vector as Vector
import qualified Graphics.Vty as V
import System.FilePath
import System.IO
import System.Posix.Files
import Text.Wrap

import StuntDouble

------------------------------------------------------------------------

readLog :: IO (Maybe Log)
readLog = do
  let commandPipe  = "/tmp" </> "scheduler-admin"
      responsePipe = "/tmp" </> "scheduler-admin-response"
  exists <- (,) <$> fileExist commandPipe <*> fileExist responsePipe
  if not (and exists)
  then return Nothing
  else do
    cfs <- getFileStatus commandPipe
    rfs <- getFileStatus responsePipe
    if not (isNamedPipe cfs && isNamedPipe rfs)
    then return Nothing
    else do
      -- NOTE: We need to start reading the response before making the request to
      -- dump the log, otherwise the response will be written to the void.
      a <- async (withFile responsePipe ReadWriteMode hGetLine)
      appendFile commandPipe "AdminDumpLog\n"
      s <- wait a
      return (Just (read s))

drawUI :: AppState -> [Widget ()]
drawUI as = [ui]
  where
    ui = withBorderStyle unicode
       $ borderWithLabel (str "Debugger")
       $ hBox [ center (L.renderList listDrawElement True (asLog as))
              , vBorder
              , center (strWrapWith wrapSettings (displaySelectedMessage as))
              ]
    wrapSettings = defaultWrapSettings
      { preserveIndentation = False, breakLongWords = True }

listDrawElement :: Bool -> Timestamped LogEntry -> Widget ()
listDrawElement sel (Timestamped le (LogicalTime (NodeName nn) lt) pt) =
  let selStr s = if sel
                 then withAttr customAttr (str $ "<" <> s <> ">")
                 else str s
  in selStr $ display le ++ " " ++ nn ++ " " ++ show lt ++ " " ++ show pt

display :: LogEntry -> String
display (LogSend (LocalRef i) (RemoteRef a j) msg)
  = show i ++ " --> " ++ show j ++ " @" ++ a
display (LogResumeContinuation (RemoteRef a i) (LocalRef j) msg)
  = show j ++ " <-- " ++ show i ++ " @" ++ a

displaySelectedMessage :: AppState -> String
displaySelectedMessage as = case L.listSelectedElement (asLog as) of
  Nothing -> "?"
  Just (_ix, Timestamped (LogSend _from _to msg) _lt _pt) -> show msg
  Just (_ix, Timestamped (LogResumeContinuation _from _to msg) _lt _pt) -> show msg

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
  { asLog :: L.List () (Timestamped LogEntry)
  }

initialState :: Log -> AppState
initialState (Log es) = AppState
  { asLog = L.list () (Vector.fromList es) 1
  }

main :: IO ()
main = do
  ml <- readLog
  case ml of
    Nothing -> putStrLn "Couldn't connect to scheduler pipe."
    Just l  -> void (defaultMain brickApp (initialState l))
