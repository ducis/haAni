PRAGMA foreign_keys = ON;
--ATTACH ':memory:' AS gl_sync;

CREATE TABLE gl_sync_geometry_impl(
	geometry_name VARCHAR,
	impl_time_start INTEGER,
	gl_vao_id INTEGER,
	UNIQUE(geometry_name,impl_time_start)
);
CREATE TABLE gl_sync_geometry_impl_attrib_groups(
	geometry_name VARCHAR,
	impl_time_start INTEGER,
	attrib_group_id INTEGER,
	stride INTEGER, -- >0
	UNIQUE(geometry_name,impl_time_start,attrib_group_id)
);
CREATE TABLE gl_sync_geometry_length(
	gl_vao_id VARCHAR,
	time_start INTEGER,
	length INTEGER,
	UNIQUE(gl_vao_id,time_start)
);
--Every geometry maps to a VAO
CREATE TABLE gl_sync_geometry_attrib_in_group(
	geometry_name VARCHAR,
	impl_time_start INTEGER,
	attrib_name VARCHAR,
	-- gl_va_id INTEGER, -- VertexAttribute
	attrib_group_id INTEGER,
	offset INTEGER, -- in bytes
	UNIQUE(geometry_name,impl_time_start,attrib_name),
--	UNIQUE(geometry_name,impl_time_start,gl_va_id),
	UNIQUE(geometry_name,impl_time_start,attrib_group_id,offset)
);
CREATE TABLE gl_sync_geometry_attrib_group_has_auto_buffers(
	geometry_name VARCHAR,
	impl_time_start INTEGER,
	attrib_group_id INTEGER,
	gl_buffer_id INTEGER,
	ref_count INTEGER,
	buffer_size INTEGER, --in bytes
	UNIQUE(geometry_name,impl_time_start,attrib_group_id,gl_buffer_id)
	UNIQUE(gl_buffer_id)
);
CREATE TABLE gl_sync_gl_vao_buffer_binding(
	gl_vao_id INTEGER,
	time_start INTEGER,
	attrib_group_id INTEGER,
	gl_buffer_id INTEGER,
	UNIQUE(time_start,gl_vao_id,attrib_group_id)
	UNIQUE(time_start,gl_vao_id,gl_buffer_id)
);
CREATE TABLE gl_sync_gl_vao_buffer_has_vas(
	gl_vao_id INTEGER,
	time_start INTEGER,
	attrib_group_id INTEGER,
	attrib_name VARCHAR,
	gl_va_id INTEGER,
	gl_vec_dim INTEGER,
	gl_type INTEGER,
	gl_normalized INTEGER, -- -1:glVertexAttribIPointer
	gl_stride INTEGER,
	gl_pointer INTEGER,
--	UNIQUE(time_start,gl_vao_id,gl_va_id,gl_pointer)
	UNIQUE(time_start,gl_vao_id,attrib_group_id,attrib_name)
	UNIQUE(time_start,gl_vao_id,attrib_group_id,gl_pointer)
);
/*CREATE TABLE gl_sync_geometry_auto_buffers_loaded(
	geometry_name VARCHAR,
	impl_time_start VARCHAR,
--	attrib_name VARCHAR,
	usage_time_start INTEGER,
	gl_buffer_id INTEGER,
--	FOREIGN KEY(geometry_name,attrib_name) REFERENCES geometry_data_attrib_def,
	UNIQUE(geometry_name,impl_time_start,attrib_name,usage_time_start)
);*/


CREATE TABLE gl_sync_draw_call_prop(
	dc_name VARCHAR, --REF draw_calls(name)
	time_start INTEGER,
	prim_type INTEGER,
	gl_vao_id INTEGER,
	instancing BOOLEAN,
	UNIQUE (dc_name,time_start)
);

CREATE TABLE gl_sync_shader_binding(
	dc_name VARCHAR,
	time_start INTEGER,
	program_id INTEGER,
	UNIQUE(time_start,dc_name)
);

CREATE TABLE gl_sync_shader_constants_m44(
	dc_name VARCHAR, --REF draw_calls(name)
	time_start INTEGER, 
	uniform_location INTEGER,
	m11 FLOAT, m12 FLOAT, m13 FLOAT, m14 FLOAT, 
	m21 FLOAT, m22 FLOAT, m23 FLOAT, m24 FLOAT, 
	m31 FLOAT, m32 FLOAT, m33 FLOAT, m34 FLOAT,
	m41 FLOAT, m42 FLOAT, m43 FLOAT, m44 FLOAT,
	UNIQUE (time_start, dc_name, uniform_location) 
);
CREATE TABLE gl_sync_shader_constants_v4(
	dc_name VARCHAR, --REF draw_calls(name)
	time_start INTEGER,
	uniform_location INTEGER, 
	v1 FLOAT, v2 FLOAT, v3 FLOAT, v4 FLOAT,
	UNIQUE (time_start, dc_name, uniform_location)
);
/*
CREATE TABLE gl.gl_geometry_explicit_buffers_current(
	geometry_name VARCHAR,
	buffer_id INTEGER,
	gl_id INTEGER,
	UNIQUE(geometry_name,buffer_id),
	UNIQUE(gl_id)
);
CREATE TABLE gl.gl_geometry_explicit_buffers_mapping(
	geometry_name VARCHAR,
	time_start INTEGER,
	buffer_id INTEGER,
	gl_id INTEGER,
	FOREIGN KEY(geometry_name,time_start,buffer_id),-- REFERENCES geometry_data_buffer,
	UNIQUE(geometry_name,time_start,buffer_id)
);*/
