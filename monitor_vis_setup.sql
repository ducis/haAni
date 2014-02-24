INSERT INTO resources VALUES
	('g','snake'),
	('s','snakeVS'),
	('s','snakeFS')
INSERT INTO draw_calls VALUES
	('dc_bts',0,9000000000000000000),
	('dc_n_p',0,9000000000000000000),
	('dc_n_s',0,9000000000000000000),
	('dc_n_b',0,9000000000000000000),
	('dc_n_v',0,9000000000000000000);
INSERT INTO draw_call_prop VALUES
	('dc_bts',0,'ls','snake',0),
	('dc_n_p',0,'ls','snake',0),
	('dc_n_s',0,'ls','snake',0),
	('dc_n_b',0,'ls','snake',0),
	('dc_n_v',0,'ls','snake',0);
INSERT INTO shader_binding VALUES
	('dc_bts',0,'snakeVS','','snakeFS'),
	('dc_n_p',0,'snakeVS','','snakeFS'),
	('dc_n_s',0,'snakeVS','','snakeFS'),
	('dc_n_b',0,'snakeVS','','snakeFS'),
	('dc_n_v',0,'snakeVS','','snakeFS');
INSERT INTO shader_constants_v4 VALUES
	('dc_bts',0,'h',0,0,0,0),
	('dc_n_p',0,'h',0,0,0,0),
	('dc_n_s',0,'h',0,0,0,0),
	('dc_n_b',0,'h',0,0,0,0),
	('dc_n_v',0,'h',0,0,0,0),
	('dc_bts',0,'color',0,0,0,0),
	('dc_n_p',0,'color',0,0,1,0),
	('dc_n_s',0,'color',0,1,0,0),
	('dc_n_b',0,'color',1,0,0,0),
	('dc_n_v',0,'color',0,1,1,0);
INSERT INTO geometry_type_attrib_def VALUES
	('snakeGeo','x','2f',1,0),
	('snakeGeo','i','2i',1,0);
INSERT INTO geometry_typing VALUES
	('snake','snakeGeo');
INSERT INTO vertex_shader_typing VALUES
	('snakeVS','snakeGeo');
INSERT INTO geometry_data_relational VALUES
	('snake','x',0,20),
	('snake','i',0,20);
INSERT INTO geometry_data VALUES
	('snake','x',0,0,-1)
