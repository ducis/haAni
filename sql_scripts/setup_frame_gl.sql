DELETE FROM cur_frame_gl_dcs;
INSERT INTO cur_frame_gl_dcs SELECT * FROM (
	(SELECT * FROM cur_frame_draw_calls)
	NATURAL JOIN
	(SELECT 	dc_name,prim_type,gl_vao_id,instancing FROM 
		(SELECT dc_name,prim_type,gl_vao_id,instancing,max(time_start) 
			FROM gl_sync_draw_call_prop WHERE ?1>=time_start GROUP BY dc_name))
	NATURAL JOIN
	(SELECT 	dc_name,program_id FROM 
		(SELECT dc_name,program_id,max(time_start) 
			FROM gl_sync_shader_binding WHERE ?1>=time_start GROUP BY dc_name))
	NATURAL JOIN
	(SELECT		gl_vao_id,0 AS prim_first,length AS prim_count FROM
		(SELECT gl_vao_id,length,max(time_start) 
			FROM gl_sync_geometry_length WHERE ?1>=time_start GROUP BY gl_vao_id))
);
