--ATTACH ':memory:' AS cur_frame_gl;
CREATE TABLE cur_frame_gl_dcs(
	dc_name VARCHAR,
	prim_type INTEGER,
	gl_vao_id INTEGER,
	instancing BOOLEAN,
	program_id INTEGER,
	prim_first INTEGER,
	prim_count INTEGER
);

