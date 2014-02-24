{-# LANGUAGE ScopedTypeVariables #-}
module HaAniRenderer where
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw
import qualified Graphics.UI.GLUT as U
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

data Renderer = Renderer DB.Database (M.Map String String)

loadPrepackedSQL = readFile "sql_scripts/idx">>=mapM f.filter (/=[]).lines
	where
	f nm = readFile ("sql_scripts/"++nm++".sql")>>=return.(,) nm

--syncGL (Renderer db ps) = sql "SELECT * FROM gl_sync_reqs_named ORDER BY time_start,type ASC LIMIT 1" []>>= mapM_ sync>>putChar '*' where
syncGL (Renderer db ps) = sql "SELECT * FROM gl_sync_reqs_named ORDER BY time_start,type ASC LIMIT 1" []>>= mapM_ sync {- >>putChar '*' -} where
	sql = runStrB db
	sync row = let 
		--[nm',ts',tp'] = row 
		--[DB.SQLText nm,DB.SQLInteger ts,DB.SQLText (_:tp)] = row
		[nm',ts',DB.SQLText (_:_:tp)] = row
		in do
		sql "UPDATE playback_control SET sync_end=?1" [ts']
		putStr "\nrow\t">>print row
		sql "DELETE FROM gl_sync_reqs_named WHERE (name=?1 AND time_start=?2 AND type=?3)" row
		putStr "tp\t">>print tp
		case tp of
			"g"-> do
				--print (nm':ts':head rs')
				[[gtp']]<-sql "SELECT geometry_type_name FROM geometry_typing WHERE geometry_name=?1" [nm']
				(vao,its')<- sql "SELECT gl_vao_id,max(impl_time_start) FROM gl_sync_geometry_impl WHERE geometry_name=?1 AND ?2>=impl_time_start" [nm',ts']
					 >>=(\rs'->case rs' of
					[(DB.SQLInteger i:t':[])]->return (fromIntegral i,t')
					[(DB.SQLNull:DB.SQLNull:[])]->do
				--its'<- sql "SELECT max(impl_time_start) FROM gl_sync_geometry_impl WHERE geometry_name=?1 AND ?2>=impl_time_start" [nm',ts']
				--	>>=(\[[t']]->if t'/=DB.SQLNull then return t' else do
						i<-F.alloca (\s->glGenVertexArrays 1 s>>peek s)
						putStrLn $ "VAO\t"++show i
						addGLObject "v" i
						sql "INSERT INTO gl_sync_geometry_impl VALUES (?,?,?)" [nm',ts',DB.SQLInteger (fromIntegral i)]
						--sql "SELECT gl_vao_id,max(impl_time_start) FROM gl_sync_geometry_impl WHERE geometry_name=?1 AND ?2>=impl_time_start" [nm',ts']>>=print
						sql ("SELECT attrib_name,scalar_type,vec_dim FROM geometry_type_attrib_def WHERE geometry_type_name=?") [gtp']
							>>=sequence_.zipWith (\n [atr',DB.SQLText (w:_),DB.SQLInteger vd]->do
								let n'=DB.SQLInteger (fromIntegral n) in do
									sql "INSERT INTO gl_sync_geometry_impl_attrib_groups VALUES(?,?,?,?)" [nm',ts',n',DB.SQLInteger ((2^(ord w-ord '0'))*vd)]
									sql "INSERT INTO gl_sync_geometry_attrib_in_group VALUES(?,?,?,?,?)" [nm',ts',atr',n',DB.SQLInteger 0]
							) [0..]
						return (i,ts')
						--return ts'
					)
				sortedAtrs'<-sql "SELECT attrib_name FROM geometry_type_attrib_def WHERE geometry_type_name=? ORDER BY attrib_name ASC" [gtp']>>=return.map head
				print (vao,its')
				-- every attrib maps to one buffer for now
				sql ("SELECT attrib_group_id,attrib_name,num_elements,scalar_type,vec_dim,offset,volatility FROM ("
					++(concat.intersperse " NATURAL JOIN ") [
					"(SELECT geometry_name,attrib_name,num_elements FROM geometry_data_relational WHERE geometry_name=?1 AND time_start=?2)",
					"(SELECT attrib_name,scalar_type,vec_dim,volatility FROM geometry_type_attrib_def WHERE geometry_type_name=?4)",
					"(SELECT geometry_name,attrib_name,attrib_group_id,offset FROM gl_sync_geometry_attrib_in_group WHERE impl_time_start=?3)"
					-- "(SELECT geometry_name,gl_va_id,stride FROM gl_sync_geometry_impl_gl_va WHERE impl_time_start=?3)"
					]++") ORDER BY attrib_group_id,offset")	[nm',ts',its',gtp']>>=(\rs'->let 
					f::(Integral a)=>a->DB.SQLData
					f = DB.SQLInteger . fromIntegral
					cI = stripSQL
					newLength = (\xs->if xs==[] then 0 else minimum xs) $ filter (>0) $ map (cI.(!!2)) rs'
					in do		
						sql "SELECT length,max(time_start) FROM gl_sync_geometry_length WHERE gl_vao_id=?1 AND ?2>=time_start" [f vao,ts']>>=(\rs'->
							case rs' of
								[(DB.SQLInteger l:_)] | l==newLength -> return ()
								_ -> {-print rs'>>-}sql "INSERT INTO gl_sync_geometry_length VALUES (?,?,?)" [f vao,ts',DB.SQLInteger newLength] >> return ()
							)
						print rs'
						mapM_ (\rows->let
							[(ag':_),atrs',nEs',sts',dims',offsets',vls'] = transpose rows
							mx' = maximum.map cI
							in do
								[[DB.SQLInteger stride]]<-sql
									"SELECT stride FROM gl_sync_geometry_impl_attrib_groups WHERE geometry_name=?1 AND impl_time_start=?2 AND attrib_group_id=?3" [nm',its',ag']
								(b,bsz)<-sql ("SELECT gl_buffer_id,buffer_size FROM gl_sync_geometry_attrib_group_has_auto_buffers WHERE "++
									"ref_count=0 AND geometry_name=?1 AND impl_time_start=?2 AND attrib_group_id=?3") [nm',its',ag']>>=(\rs'->case rs' of
									[[DB.SQLInteger b,DB.SQLInteger bsz]]->do
										print (b,bsz)
										sql ("UPDATE gl_sync_geometry_attrib_group_has_auto_buffers SET ref_count=1 WHERE "++
											"geometry_name=?1 AND impl_time_start=?2 AND attrib_group_id=?3") [nm',its',ag']
										return (fromIntegral b, fromIntegral bsz)
									[]->do
										b<-F.alloca (\s->glGenBuffers 1 s>>peek s)
										print (b,0)
										sql "INSERT INTO gl_sync_geometry_attrib_group_has_auto_buffers VALUES (?,?,?,?,1,0)" [nm',its',ag',DB.SQLInteger (fromIntegral b)]
										return (b,0)
									)
								print (b,bsz)
								glBindBuffer gl_ARRAY_BUFFER b
								p::IntPtr<-let sz0 = fromIntegral $ mx' nEs'*stride in do
									when (bsz<sz0) $ do
										sql ("UPDATE gl_sync_geometry_attrib_group_has_auto_buffers SET buffer_size=?1 "++
											"WHERE geometry_name=?2 AND impl_time_start=?3 AND attrib_group_id=?4") [DB.SQLInteger (fromIntegral sz0),nm',its',ag']
										glBufferData gl_ARRAY_BUFFER sz0 nullPtr $ [gl_STATIC_DRAW,gl_DYNAMIC_DRAW,gl_STREAM_DRAW] !! (fromIntegral $ mx' vls')
									--p<-glMapBuffer gl_ARRAY_BUFFER gl_WRITE_ONLY
									glMapBufferRange gl_ARRAY_BUFFER 0 sz0 (fromIntegral gl_MAP_WRITE_BIT.|.fromIntegral gl_MAP_INVALIDATE_BUFFER_BIT) >>= return.ptrToIntPtr
								--TODO:screen mode change
								--TODO:partial update
								sql "INSERT INTO gl_sync_gl_vao_buffer_binding VALUES(?,?,?,?)" [f vao,ts',ag',f b]
								mapM_ (\[_,atr',DB.SQLInteger nE,DB.SQLText (w:t:rm),DB.SQLInteger vd,DB.SQLInteger offset,_]->
									sql "SELECT i,value FROM geometry_data WHERE geometry_name=?1 AND attrib_name=?2 AND time_start=?3 ORDER BY i ASC" [nm',atr',ts']
									>>=mapM_ (\[DB.SQLInteger i,val']->let
										pp = (p + (fromIntegral $ offset+hi*stride+lo*ss) ) 
										(hi,lo) = divMod i vd 
										ss = 2^(ord w-ord '0') in do
										--printf "##%d\n" (fromIntegral pp::Int)
										case t of
											'f'->let DB.SQLFloat v = val' in case w of
												'2'->poke (intPtrToPtr pp) (realToFrac v::Float)
												'3'->poke (intPtrToPtr pp) (realToFrac v::Double)
											's'->let DB.SQLInteger v = val' in case w of
												'0'->poke (intPtrToPtr pp) (fromIntegral v::Int8)
												'1'->poke (intPtrToPtr pp) (fromIntegral v::Int16)
												'2'->poke (intPtrToPtr pp) (fromIntegral v::Int32)
											'u'->let DB.SQLInteger v = val' in case w of
												'0'->poke (intPtrToPtr pp) (fromIntegral v::Word8)
												'1'->poke (intPtrToPtr pp) (fromIntegral v::Word16)
												'2'->poke (intPtrToPtr pp) (fromIntegral v::Word32)
									)>> let 
										glt = case [w,t] of {"0u"->gl_UNSIGNED_BYTE;"1u"->gl_UNSIGNED_SHORT;"2u"->gl_UNSIGNED_INT;
											"0s"->gl_BYTE;"1s"->gl_SHORT;"2s"->gl_INT;"1f"->gl_HALF_FLOAT;"2f"->gl_FLOAT;"3f"->gl_DOUBLE}
										norm = case (t:rm) of
											('f':_)->0
											(_:"f")->0
											(_:"fn")->1
											(_:[])->(-1)
										Just i = elemIndex atr' sortedAtrs'
										in sql "INSERT INTO gl_sync_gl_vao_buffer_has_vas VALUES (?,?,?,?,?,?,?,?,?,?)" [f vao,ts',ag',atr',f i,f vd,f glt,f norm,f stride,f offset]
									) rows
								glUnmapBuffer gl_ARRAY_BUFFER
								printf "BUFFER LOADED %d" (fromIntegral b::Int)
								return ()
							) $ groupBy (\(a:_) (b:_)->a==b) rs'
						)
				return ()
			_-> do
				src<-sql ("SELECT * FROM "++
					(case tp of {"p"->"draw_call_prop";
						"s"->"shader_binding";"cm"->"shader_constants_m44";"cv"->"shader_constants_v4"})
					++" WHERE dc_name=?1 AND time_start=?2") [nm',ts']
				putStr "src\t">>print src
				case tp of 
					"p"-> let 
						[(_:_:DB.SQLText ptp:gn':xs')]=src 
						gptp = case ptp of 
							"p"->gl_POINTS
							"l"->gl_LINES
							"ls"->gl_LINE_STRIP
							"ll"->gl_LINE_LOOP
							"t"->gl_TRIANGLES
							"tf"->gl_TRIANGLE_FAN
							"ts"->gl_TRIANGLE_STRIP
						in void $ do
							[(vao':_)]<-sql "SELECT gl_vao_id,max(impl_time_start) FROM gl_sync_geometry_impl WHERE geometry_name=?1 AND ?2>=impl_time_start" [gn',ts']
							--putStr "+++++++++ ">>print [nm',ts',vao']
							sql "INSERT INTO gl_sync_draw_call_prop VALUES (?,?,?,?,?)" (nm':ts':DB.SQLInteger (fromIntegral gptp):vao':xs')
					"s"-> let [(_:_:ss')]=src in void $ do
						sids'<-zipWithM (\a b->if b==DB.SQLText "" then return (DB.SQLInteger (-1)) else syncShader a b) ["v","g","f"] ss'
						rs'<-sql "SELECT program_id FROM gl_programs WHERE vs_id=?1 AND gs_id=?2 AND fs_id=?3" sids'
						pid'<-case rs' of
							[[r']]->return r'
							[]->do
								pid<-glCreateProgram
								mapM_ (\(DB.SQLInteger i)->when (i>=0) $ glAttachShader pid $ fromIntegral i) sids'
								--putStrLn "+++++"
								--sql "SELECT geometry_type_name,attrib_name FROM geometry_type_attrib_def" []>>=print
								--sql "SELECT geometry_type_name FROM vertex_shader_typing WHERE vertex_shader_name=?1"[ss'!!0]>>=print
								sql ("SELECT attrib_name FROM ((SELECT geometry_type_name FROM vertex_shader_typing WHERE vertex_shader_name=?1) "++
									"NATURAL JOIN (SELECT geometry_type_name,attrib_name FROM geometry_type_attrib_def)) ORDER BY attrib_name ASC") [ss'!!0]
									>>=zipWithM_ (\i [DB.SQLText atr]->withCString atr (glBindAttribLocation pid i.castPtr)) [0..]
								mapM_ (\f->f pid) [glLinkProgram,glValidateProgram]
								--get (validateStatus $ Program pid)>>=print
								get (programInfoLog $ Program pid) >>= putStrLn.("building program... "++)
								let r'=DB.SQLInteger $ fromIntegral pid
								sql "INSERT INTO gl_programs VALUES (?1,?2,?3,?4)" $ sids'++[r']
								return r'
						sql "INSERT INTO gl_sync_shader_binding VALUES (?1,?2,?3)" [nm',ts',pid']
					('c':t)-> void $ do
						[(DB.SQLInteger i:_)]<-sql "SELECT program_id,max(time_start) FROM gl_sync_shader_binding WHERE dc_name=?1 AND ?2>=time_start" [nm',ts']
						mapM_ (\(_:_:DB.SQLText u:xs')->(do
							uid<-withCString u (glGetUniformLocation (fromIntegral i).castPtr)
							printf "uniform location %d" (fromIntegral uid::Int)
							sql ("INSERT INTO gl_sync_shader_constants_"++(case t of {"m"->"m44";"v"->"v4"})
								++" VALUES (?,?,?"++concat (map (\_->",?") xs')++")") (nm':ts':DB.SQLInteger (fromIntegral uid):xs')
							)) src
	syncShader shaderType nm' = syncNamedGLObject (DB.SQLText "s") (loadShader shaderType nm') nm'
	syncNamedGLObject tp' objLoader nm' = do --loader should take care of updating gl_objects 
		rs'<-sql "SELECT gl_id FROM gl_object_naming WHERE type=?1 AND name=?2" [tp', nm']
		case rs' of
			[[id']] -> return id'
			[] -> do
				id'<-objLoader>>=return.DB.SQLInteger
				sql "INSERT INTO gl_object_naming VALUES (?1,?2,?3)" [tp',id',nm']
				return id'
	addGLObject tp id = sql "INSERT INTO gl_objects VALUES (?1,?2)" [DB.SQLText tp,DB.SQLInteger $ fromIntegral id]
	loadShader sTp nm' = do
		--get vendor>>=putStrLn.(++"###")
		--glGetString gl_VERSION>>=print --peekCString.castPtr
		id<-glCreateShader (case sTp of {"v"->gl_VERTEX_SHADER;"g"->gl_GEOMETRY_SHADER;"f"->gl_FRAGMENT_SHADER})>>=return.fromIntegral
		--id<-case sTp of {"v"->(genObjectNames 1>>=return.vertexShaderID.head)}>>=return.fromIntegral
		(\s->	acqTextRes sql "s" nm'>>=(shaderSource s$=).(:[])>>
			compileShader s>>putStrLn ("Compiling shader "++(stripSQL nm'))>>get (compileStatus s)>>=print
			>>get (shaderInfoLog s)>>=putStrLn ) (FragmentShader $ fromIntegral id)
		addGLObject "s" id
		sql "INSERT INTO gl_shaders VALUES (?1,?2)" [DB.SQLInteger id,DB.SQLText sTp]
		putStrLn $ printf "loaded shader\t%s %d" ((\(DB.SQLText x)->x)nm') id
		return id

acqTextRes sql tp nm' = sql "SELECT data FROM text_res WHERE type=?1 AND name=?2" [DB.SQLText tp,nm']>>=(\[[DB.SQLText r]]->return r)
acqBinRes sql tp nm' = sql "SELECT data FROM bin_res WHERE type=?1 AND name=?2" [DB.SQLText tp,nm']>>=(\[[DB.SQLBlob r]]->return r)

--stepFrameGL (Renderer db ps) (t::Int64) = return ()
playbackGL (Renderer db ps) = do
	[f,j]<-runStr db "SELECT force_to_play,just_played FROM playback_control">>=return.map (\(DB.SQLInteger i)->i).head
	let i = (if f<0 then j+1 else f) in do
		runStrB db "UPDATE playback_control SET just_played=?1" [DB.SQLInteger i]
		showFrameGL (Renderer db ps) i
		
	
showFrameGL (Renderer db ps) (t::Int64) = do
	mapM (\(s,b)->DB.runScript db (ps M.! s) b) [
		("setup_frame",		[t']),
		("setup_frame_gl",	[t'])
		]
	glClearColor 1 1 1 0
	clear [ColorBuffer,DepthBuffer]
	sql "SELECT gl_vao_id,program_id,dc_name,prim_type,instancing,prim_first,prim_count FROM cur_frame_gl_dcs ORDER BY gl_vao_id,program_id" [] >>= mapM_ (\rows->
		let 
			((vao':_):_)=rows 
			DB.SQLInteger vao = vao'
			in do
			glBindVertexArray $ cI vao
			--print ("glBindVertexArray",vao)
			sql ("SELECT attrib_group_id,gl_buffer_id,max(time_start) FROM gl_sync_gl_vao_buffer_binding "++ 
				"WHERE gl_vao_id=?1 AND ?2>=time_start GROUP BY attrib_group_id") [vao',t']>>=mapM_
				(\[ag',DB.SQLInteger b,ts']->do
					glBindBuffer gl_ARRAY_BUFFER $ fromIntegral b
					--print ("glBindBuffer",b)
					as<-sql ("SELECT gl_vec_dim,gl_type,gl_normalized,gl_stride,gl_pointer,gl_va_id FROM gl_sync_gl_vao_buffer_has_vas "++
						"WHERE gl_vao_id=?1 AND time_start=?2 AND attrib_group_id=?3 ORDER BY attrib_name ASC") [vao',ts',ag']
					mapM_ (\[vd',glt',nrm',strd',p',va']->let 
						c::(Integral a)=>DB.SQLData->a
						c=cSI
						i=c va'
						in glEnableVertexAttribArray i {- <- this actually needs to be called once every geometry -} >> (case c nrm' of
							-1 -> glVertexAttribIPointer i (c vd') (c glt')
							_ -> glVertexAttribPointer i (c vd') (c glt') (c nrm')
							) (c strd') (intPtrToPtr $ c p') -- >> print ("glVertexAttrib*Pointer",i,vd',glt',strd',p')
						) as
					return $ map head as
				)
			--print rows
			mapM_ (\rows0->let ((p':_):_) = rows0 in do
				glUseProgram (cSI p') -- >> print("glUseProgram",p')
				--print rows0
				mapM_ (\[dcn',pt',ins',start',count']->let 
					mode = cSI pt'
					a = cSI start'
					b = cSI count'
					f n ss' = map (\(DB.SQLFloat x)->realToFrac x) $ take n ss'
					sqlS suffix = sql ("SELECT *,max(time_start) FROM gl_sync_shader_constants_"++suffix++" WHERE dc_name=?1 AND ?2>=time_start GROUP BY uniform_location") [dcn',t']
					in do
					--sqlS "m44" >>=print
					--sqlS "v4" >>=print
					sqlS "m44" >>=mapM_ (\(_:_:ul':rem')->F.withArray (f 16 rem') (glUniformMatrix4fv (cSI ul') 1 (fromIntegral gl_TRUE)))
					sqlS "v4" >>=mapM_ (\(_:_:ul':rem')->F.withArray (f 4 rem') (glUniform4fv (cSI ul') 1 ))
					if cSI ins'==0 then 
						glDrawArrays mode a b
					else
						sql "SELECT instances,max(time_start) FROM instancing WHERE dc_name=?1 AND ?2>=time_start" [dcn',t']>>=
							glDrawArraysInstanced mode a b.(\[(is':_)]->cSI is')
					) $ map tail rows0
				) (groupH $ map tail rows)
		).groupH
	where 
	cSI::(Integral a)=>DB.SQLData->a
	cSI = fromIntegral.(\(DB.SQLInteger i)->i)
--	fSI = fromIntegral.(\(DB.SQLInteger (i::(Integral a)=>a))->i)
	cI::(Integral a,Integral b)=>a->b
	cI = fromIntegral
	groupH = groupBy (\(a:_) (b:_)->a==b)
--	eqHead (a:_) (b:_) = a==b
	txt = DB.SQLText
	sql = runStrB db
	t' = DB.SQLInteger $ fromIntegral t
	--draw dc = let (name':_) = dc in do
	--	print dc



