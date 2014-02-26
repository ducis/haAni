{-# LANGUAGE ScopedTypeVariables #-}
module Main (Main.main) where
import GHC.IO.Handle
import GHC.IO.Handle.FD
import Graphics.UI.Gtk
import qualified Database.SQLite3 as DB
import SQLUtil
import System.Environment
import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad

main :: IO ()
main = do
  initGUI
  window <- windowNew
  onDestroy window mainQuit
  set window [ containerBorderWidth := 10, windowTitle := "Controller" ]
  btnLoad <- buttonNew
  set btnLoad [ buttonLabel := "Load Script" ]
  [btnPlay,btnPause] <- mapM (buttonNewWithLabel) ["Play","Pause"]
  hbb <- hBoxNew True 8--hButtonBoxNew
  set hbb $ [ containerChild := b | b<-[btnLoad,btnPlay,btnPause] ]
  adj <- adjustmentNew 0 0 1 1 1 1
  scl <- hScaleNew adj
  vbb <- vBoxNew True 4
  set vbb $ [ containerChild:=hbb, containerChild:=scl] --, containerChild:=ctxt]
  set window [containerChild:=vbb]
  widgetShowAll window
  db<-getArgs>>=mawaru.head
  timeoutAddFull (checkPlayback db adj>>return True) priorityDefaultIdle 10
  onClicked btnLoad $ openOpenFileDialog window db
  onClicked btnPause $ freezePlayer db adj
  onClicked btnPlay $ void $ runStr db "UPDATE playback_control SET force_to_play=(-1)"
  mainGUI

  
openOpenFileDialog parentWindow db = do
  dialog <- fileChooserDialogNew
              (Just "Open")             
              (Just parentWindow)       
              FileChooserActionOpen     
              [("gtk-cancel"            
               ,ResponseCancel)
              ,("gtk-open"
               , ResponseAccept)]
  widgetShow dialog
  response <- dialogRun dialog
  case response of
    ResponseAccept -> do 
		Just fileName <- fileChooserGetFilename dialog
		putStrLn $ "SCR: " ++ show fileName
		readFile fileName>>=(\s->DB.runScript db s [])
		hFlush stdout
    _->return ()
  widgetHide dialog

--forgive::(Monad a) => a->a
forgive = (\m->E.catch m (\(e::E.SomeException)->putStr "w">> hFlush stdout>>threadDelay 100000))

mawaru s = try>>=decide
	where 
	try = do 
		db<-DB.open s
		E.catch (runStr db "SELECT * FROM playback_control">>return (Just db))
			(\(e::E.SomeException)->threadDelay 100000>>DB.close db>>return Nothing)
	decide (Just db) = return db
	decide Nothing = try>>=decide

freezePlayer db adj = do
	i<-adjustmentGetValue adj
	runStrB db "UPDATE playback_control SET force_to_play=?1" [DB.SQLInteger $ truncate i]
	return ()

checkPlayback db adj = do
	[s,e,f,j]<- runStr db "SELECT sync_start,sync_end,force_to_play,just_played FROM playback_control"
		>>=return.map (\(DB.SQLInteger i)->fromIntegral i).head
--	print [s,e,f,j] >> hFlush stdout
	adjustmentSetLower adj s
	adjustmentSetUpper adj e
	if (f<0) then
		adjustmentSetValue adj j
	else
		freezePlayer db adj
