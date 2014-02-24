--DROP TABLE IF EXISTS cur_frame.draw_calls;
DELETE FROM cur_frame_draw_calls;
--CREATE TABLE cur_frame.draw_calls AS 
INSERT INTO cur_frame_draw_calls
	SELECT name AS dc_name FROM draw_calls WHERE ?1>=time_start AND ?1<time_end;
/*DROP TABLE IF EXISTS cur_frame.full_dcs;
DROP TABLE IF EXISTS cur_frame.instancing;
DROP TABLE IF EXISTS cur_frame.shader_constants_m44;
DROP TABLE IF EXISTS cur_frame.shader_constants_v4;
CREATE TABLE cur_frame.full_dcs AS
	SELECT * FROM
	(SELECT * FROM cur_frame.draw_calls)
	NATURAL JOIN
	(SELECT dc_name,prim_type,geometry_name,instancing 
		FROM draw_call_prop WHERE ?1>=time_start ORDER BY time_start DESC LIMIT 1)
	NATURAL JOIN
	(SELECT dc_name,vertex_shader_name,geometry_shader_name,fragment_shader_name 
		FROM shader_binding WHERE ?1>=time_start ORDER BY time_start DESC LIMIT 1)
	ORDER BY vertex_shader_name,geometry_shader_name,fragment_shader_name,geometry_name;
CREATE TABLE cur_frame.instancing AS SELECT dc_name,instances FROM instancing;
CREATE TABLE cur_frame.shader_constants_m44 AS
	SELECT dc_name,var_name,
		m11,m12,m13,m14,
		m21,m22,m23,m24,
		m31,m32,m33,m34,
		m41,m42,m43,m44
	FROM shader_constants_m44 WHERE ?1>=time_start ORDER BY time_start DESC LIMIT 1;
CREATE TABLE cur_frame.shader_constants_v4 AS
	SELECT dc_name,var_name,v1,v2,v3,v4
	FROM shader_constants_v4 WHERE ?1>=time_start ORDER BY time_start DESC LIMIT 1;
*/
