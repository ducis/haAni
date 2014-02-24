{-# LANGUAGE ScopedTypeVariables #-}
import qualified Graphics.UI.GLUT as U
import Graphics.Rendering.OpenGL
import Data.Bits
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.String
import qualified Foreign.Marshal as F
import Data.Char
import SQLUtil
import qualified Database.SQLite3 as DB
import GHC.IO.Handle
import GHC.IO.Handle.FD
import qualified Data.Vec as V
import qualified Data.Vec.LinAlg.Transform3D as V
import Text.Printf
import qualified Data.Map as M
import Data.Int
import Control.Concurrent
import Control.Monad
import Data.List
import Data.Word
import Data.Int
import HaAniRenderer

main = do
	(_,[wS,hS,deltaTS,dbS])<-U.getArgsAndInitialize
	putStrLn dbS
	db<-DB.open dbS
--	DB.runScript db "ATTACH ':memory:' AS cur_frame; ATTACH ':memory' AS cur_frame_gl;" []
--	runScript db "CREATE TABLE cur_frame_fasdfas(id INT);CREATE TABLE cur.dfawre(id INT);"
--	DB.runScript db "ATTACH ':memory:' AS cur_frame; CREATE TABLE cur_frame_fasdfas(id INT);CREATE TABLE cur.dfawre(id INT);" []
	--putStr "\n">>runStr db "SELECT * FROM sqlite_master" >>= sequence.map print>>putStr "\n"

	U.initialWindowSize $= U.Size (read wS) (read hS)
	U.initialDisplayMode $= [U.RGBAMode,U.WithAlphaComponent,U.WithDepthBuffer,U.DoubleBuffered]
	wnd<-U.createWindow "Player"
	U.displayCallback $= return ()
	prepackedSQL <- loadPrepackedSQL>>=return.M.fromList

	mapM (\s->DB.runScript db (prepackedSQL M.! s) []) 
		["main","gl","gl_sync","gl_sync_req","cur_frame","cur_frame_gl","playback"]
	cp "DB Initialized."
	--forkIO $ void $ readFile sceneScript>>=(\s->DB.runScript db s [])
	forkIO $ void $ getContents>>=(\s->DB.runScript db s [])
--	cp "Loaded."

	mapM_ (\x->get x>>=putStrLn) [vendor,renderer,glVersion,shadingLanguageVersion]

	let rndr = (Renderer db prepackedSQL)
	U.addTimerCallback 0 $ step rndr 0 (fromIntegral (read deltaTS))
	forkOS $ forever $ hFlush stdout>>threadDelay 2100000
	-- forkOS (forever $ syncGL rndr)
	U.idleCallback $= Just (syncGL rndr)
	depthFunc$=Just Lequal
--	U.actionOnWindowClose $= U.Exit
	U.mainLoop
	where
	cp s = putStrLn s>>hFlush stdout
	step rndr i t = do
			U.addTimerCallback t (step rndr (i+1) t)
			playbackGL rndr--showFrameGL rndr i
			U.swapBuffers
			U.reportErrors
