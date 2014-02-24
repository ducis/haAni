PRAGMA foreign_keys = ON;
--ATTACH ':memory:' AS gl;

CREATE TABLE gl_objects(
	type CHAR, --'p':program,'s':shader,'b':buffer,'v':VAO
	gl_id INTEGER,
	UNIQUE (type,gl_id)
);
CREATE TABLE gl_object_naming(
	type CHAR, --'s','b','v'
	gl_id INTEGER,
	name VARCHAR,
--	FOREIGN KEY (type,gl_id) REFERENCES gl_objects,
	UNIQUE (type,gl_id),
	UNIQUE (type,name)
);
CREATE TABLE gl_buffer_objects( 
	gl_id INTEGER, --REF gl_objects
	type CHAR, --'a':array,'e':element_array,'t':texture
	UNIQUE (gl_id)
);
CREATE TABLE gl_shaders(
	gl_id INTEGER, --REF gl_objects
	type CHAR, --'v','g','f'
	UNIQUE (gl_id)
);
CREATE TABLE gl_programs(
	vs_id INTEGER, --REF gl_objects(gl_id),
	gs_id INTEGER, --REF gl_objects(gl_id),
	fs_id INTEGER, --REF gl_objects(gl_id),
	program_id INTEGER, --REF gl_objects(gl_id),
	UNIQUE (vs_id, gs_id, fs_id),
	UNIQUE (program_id)
);
/*CREATE TABLE gl_uniforms(
	program_id INTEGER REFERENCES gl_programs,
	var_name VARCHAR,
	uniform_location INTEGER,
	UNIQUE (program_id,var_name),
	UNIQUE (program_id,uniform_location)
);
CREATE TABLE gl_vertex_attrib(
	program_id INTEGER REFERENCES gl_programs,
	idx INTEGER,
	name VARCHAR,
	UNIQUE(program_id,idx),
	UNIQUE(program_id,name)
);*/
--run-time relations, which depends on current OpenGL states
--Don't use glVertexAttrib* which is like DrawPrimitivesUP
--Always use memory mapping to avoid temporary buffers in user space
--What about buffer swapping when streaming from user space?
--We don't want to read back states set previously from OpenGL.*/
