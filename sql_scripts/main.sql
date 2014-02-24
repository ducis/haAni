CREATE TABLE resources(
	type CHAR, --'s':shader, 'g': geometry
	name VARCHAR,
	UNIQUE(type,name)
);
CREATE TABLE draw_calls(
	name VARCHAR,
	time_start INTEGER,
	time_end INTEGER, --can be NULL( represented by a negtive number )
	UNIQUE (name,time_start)
);
CREATE TABLE draw_call_prop(
	dc_name VARCHAR, --REF draw_calls(name)
	time_start,
	prim_type CHAR(2), --"p":point,"l":lines,"ls":line strip,"ll":line loop,"t":tris,"tf":tri fan,"ts":tri strip
	geometry_name VARCHAR, --REF resources(name),
	instancing BOOLEAN, 
	UNIQUE (dc_name,time_start)
);
CREATE TABLE shader_binding( --there is always shader binding, so no time_end is needed.
	dc_name VARCHAR, --REF draw_calls(name),
	time_start INTEGER,
	vertex_shader_name VARCHAR, --REF resources(name),
	geometry_shader_name VARCHAR, --REF resources(name),
	fragment_shader_name VARCHAR, --REF resources(name),
	UNIQUE (time_start,dc_name)
);
CREATE TABLE instancing( --0 turns off instancing. Number of instances would change more frequently than draw_call_prop, so a separate relation is needed.
	dc_name VARCHAR, --REF draw_calls(name),
	time_start INTEGER, 
	instances INTEGER,
	UNIQUE (time_start,dc_name)
);
CREATE TABLE shader_constants_m44(
	dc_name VARCHAR, --REF draw_calls(name)
	time_start INTEGER,
	var_name VARCHAR,
	m11 FLOAT, m12 FLOAT, m13 FLOAT, m14 FLOAT, 
	m21 FLOAT, m22 FLOAT, m23 FLOAT, m24 FLOAT, 
	m31 FLOAT, m32 FLOAT, m33 FLOAT, m34 FLOAT,
	m41 FLOAT, m42 FLOAT, m43 FLOAT, m44 FLOAT,
	UNIQUE (time_start, dc_name, var_name) 
);
CREATE TABLE shader_constants_v4(
	dc_name VARCHAR, --REF draw_calls(name)
	time_start INTEGER, 
	var_name VARCHAR, 
	v1 FLOAT, v2 FLOAT, v3 FLOAT, v4 FLOAT,
	UNIQUE (time_start, dc_name, var_name)
);
CREATE TABLE geometry_type_attrib_def( --if attrib_name is empty then it's index data
	geometry_type_name VARCHAR, --REF resources(name),
	attrib_name VARCHAR,
	scalar_type CHAR, --<log 2 of scalar_size><t>,<t>::='f':floating point,'s'|'u':fixed point,'sf','uf','sfn','ufn'
	vec_dim INTEGER,
	volatility INTEGER,
	UNIQUE (geometry_type_name,attrib_name)
);
CREATE TABLE geometry_typing(
	geometry_name VARCHAR,
	geometry_type_name VARCHAR,
	UNIQUE (geometry_name)
);
CREATE TABLE vertex_shader_typing(
	vertex_shader_name VARCHAR,
	geometry_type_name VARCHAR,
	UNIQUE (vertex_shader_name)
);
CREATE TABLE geometry_data_relational( --(geometry_name,attrib_name) REF geometry_data_attrib_def,
	geometry_name VARCHAR,
	attrib_name VARCHAR,
	time_start INTEGER,
	num_elements INTEGER, --counting every vector as 1 element. 0 means not to change the buffer size.
--	partial INTEGER, --0,1,2
	UNIQUE(geometry_name,attrib_name,time_start) 
);
CREATE TABLE geometry_data(
	geometry_name VARCHAR,
	attrib_name VARCHAR,
	time_start INTEGER,
	i INTEGER, --vector data is expanded
	value, --int or float
	--FOREIGN KEY(geometry_name,attrib_name,time_start) REFERENCES geometry_data_relational,
	UNIQUE(geometry_name,attrib_name,time_start,i)
);
/*
CREATE TABLE geometry_data_int(
	geometry_name VARCHAR,
	attrib_name VARCHAR,
	time_start INTEGER,
	i INTEGER, --vector data is expanded
	value INTEGER,
	--FOREIGN KEY(geometry_name,attrib_name,time_start) REFERENCES geometry_data_relational,
	UNIQUE(geometry_name,attrib_name,time_start,i)
);
CREATE TABLE geometry_data_float(
	geometry_name VARCHAR,
	attrib_name VARCHAR,
	time_start INTEGER,
	i INTEGER, --vector data is expanded
	value FLOAT,
	--FOREIGN KEY(geometry_name,attrib_name,time_start) REFERENCES geometry_data_relational,
	UNIQUE(geometry_name,attrib_name,time_start,i)
);*/
/*
CREATE TABLE geometry_data_attrib_def_from_buffer(
	geometry_name VARCHAR,
	attrib_name VARCHAR,
	time_start INTEGER,
	buffer_id INTEGER, --REF geometry_data_buffer
	stride_in_bytes INTEGER,
	FOREIGN KEY(geometry_name,attrib_name,time_start) REFERENCES geometry_data_attrib_def,
	UNIQUE(geometry_name,attrib_name,time_start,buffer_id)
);
CREATE TABLE geometry_data_buffer(
	geometry_name VARCHAR, --REF resources(name),
	time_start INTEGER,
	buffer_id INTEGER,
	data BLOB,
	UNIQUE (geometry_name,time_start,buffer_id)
);*/
CREATE TABLE text_res( --including shader codes
	type CHAR,
	name VARCHAR,
	data VARCHAR,
	UNIQUE(type,name)
);
CREATE TABLE bin_res(
	type CHAR,
	name VARCHAR,
	data BLOB,
	UNIQUE(type,name)
);
CREATE TABLE file_res(
	type CHAR,
	name VARCHAR,
	filename VARCHAR,
	UNIQUE(type,name)
);
/*CREATE TABLE geometry_data_int_sv(
	geometry_name VARCHAR,
	attrib_name VARCHAR,
	time_start INTEGER,
	i INTEGER, --vector data is expanded
	value INTEGER,
	UNIQUE(geometry_name,attrib_name,time_start,i)
);
CREATE TABLE geometry_data_float_sv(
	geometry_name VARCHAR,
	attrib_name VARCHAR,
	time_start INTEGER,
	i INTEGER, --vector data is expanded
	value FLOAT,
	UNIQUE(geometry_name,attrib_name,time_start,i)
);
*/
