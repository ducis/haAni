{-# LANGUAGE ScopedTypeVariables #-}
module Main (Main.main) where
import GHC.IO.Handle
import GHC.IO.Handle.FD
import qualified Database.SQLite3 as DB
import SQLUtil
import System.Environment
import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import System.Time

main :: IO ()
main = do
--	[dbSubject,dbShow,dbLog]<-getArgs>>=mapM mawaru.take 3
	[dbSubjectS,dbLogS]<-getArgs
	dbSubject<-mawaru dbSubjectS
	dbLog<-DB.open dbLogS
--	readFile "monitor_vis_setup.sql">>=(\s->DB.runScript dbShow s [])
	readFile "monitor_log_setup.sql">>=(\s->DB.runScript dbLog s [])
	putStrLn "This is the MONITOR"
	hFlush stdout
	forever (threadDelay 100000>>showStats dbSubject dbLog)
	
showStats dbSubject dbLog = do
--	print 111
	hFlush stdout
	tm'<-getClockTime>>=return.DB.SQLInteger .fromIntegral.(\(TOD secs psecs)->secs*1000+(psecs `div` (10^9)))
	sql dbSubject "SELECT sum(buffer_size) FROM gl_sync_geometry_attrib_group_has_auto_buffers" []
		>>=(\[[bts']]->sql dbLog "INSERT INTO buffer_total_size VALUES (?1,?2)" [tm',bts']>>print bts')
	sql dbSubject "SELECT type,count(gl_id) FROM gl_objects GROUP BY type" []
		>>=(\rows->mapM_ (sql dbLog "INSERT INTO num_gl_objects VALUES (?,?,?)".(tm':)) rows>>print rows)
{-	sql dbLog "SELECT val FROM buffer_total_size ORDER BY time DESC LIMIT 19" []
		>>=visualize tm' "dc_bts"
	
	sequence_ $ zipWith (\tp dcn->sql dbLog "SELECT val FROM num_gl_objects WHERE type=? ORDER BY time DESC LIMIT 19"
		[DB.SQLText tp]>>=visualize tm' dcn) ["p","s","b","v"] ["dc_n_p","dc_n_s","dc_n_b","dc_n_v"]

	sql dbShow "UPDATE playback_countrol SET force_to_play=?1" [tm']-}
	where
	sql = runStrB
{-	visualize tm' dcName rows = let 
		xs = map (\[DB.SQLInteger i]->fromIntegral i) rows
		hs = map (/maximum xs) xs
		in mapM_ (\h->sql dbShow "INSERT INTO shader_constants_v4 VALUES (?,?,'h',?,0,0,0)" [DB.SQLText dcName,tm',DB.SQLFloat h]) hs
-}









mawaru s = try>>=decide
	where 
	try = do 
		db<-DB.open s
		E.catch (runStr db "SELECT * FROM playback_control">>return (Just db))
			(\(e::E.SomeException)->threadDelay 100000>>DB.close db>>return Nothing)
	decide (Just db) = return db
	decide Nothing = try>>=decide

