{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances #-}
module SQLUtil where 
import Database.SQLite3
import Foreign
import Foreign.C
import Data.List
import Foreign
import Foreign.C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.UTF8 as UTF8
import GHC.IO.Handle
import GHC.IO.Handle.FD

runStr db s = runStrB db s []
runStrB::Database->String->[SQLData]->IO [[SQLData]]
runStrB db s bs = do
	stmt<-prepare db s
	bind stmt bs
	rs<-finishStmt stmt
	finalize stmt
	return rs
finishStmt stmt = step stmt>>=d where
	d Row = do
		r<-columns stmt
		step stmt >>= d >>= (\rs->return (r:rs))
	d Done = return []
runStmtB stmt bs = {-print 1>>hFlush stdout>>-}reset stmt>>bind stmt bs>>finishStmt stmt
runStmt stmt = runStmtB stmt []

class InSQLData a where
	stripSQL :: SQLData->a
instance InSQLData Int64 where
	stripSQL (SQLInteger x)=x
instance InSQLData Double where
	stripSQL (SQLFloat x)=x
instance InSQLData String where
	stripSQL (SQLText x)=x
instance InSQLData BS.ByteString where
	stripSQL (SQLBlob x)=x
{-
runStmts stmts = runStmtsB stmts []
runScript db s = runScriptB db s []

--runScriptB should prepare statement (n+1) after executing statement n !
runScriptB::Database->String->[SQLData]->IO [[[SQLData]]]
runScriptB db s bs = prepareM db s>>=(\stmts->runStmtsB stmts bs)

runStmtsB::[Statement]->[SQLData]->IO [[[SQLData]]]
runStmtsB stmts bs = do  
	--print 2>>hFlush stdout
	sequence $ map (\s->(cnt s>>=(\n->runStmtB s (take n bs)))) stmts 
	where
	cnt s = return 0

--avoid String->[String] with sqlite3_complete
foreign import ccall "sqlite3_bind_parameter_count" sqlite3_bind_parameter_count :: Ptr ()->IO Int
-}
