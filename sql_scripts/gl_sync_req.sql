CREATE TABLE gl_sync_reqs_named(
	name VARCHAR,
	time_start INTEGER,
	type CHAR(8),
	UNIQUE (time_start,type,name) /*
		'10g':geometry_data_relational,geometry_data_buffer
		'80p':draw_call_prop, 
		'20s':shader_binding, '21cm':shader_constants_m44, '21cv':shader_constants_v4
		*/
);
CREATE TRIGGER gl_sync_reqs_on_draw_call_prop AFTER INSERT ON draw_call_prop
BEGIN 
	INSERT INTO gl_sync_reqs_named VALUES (NEW.dc_name,NEW.time_start,'80p');
END;
CREATE TRIGGER gl_sync_reqs_on_shader_binding AFTER INSERT ON shader_binding
BEGIN
	INSERT INTO gl_sync_reqs_named VALUES (NEW.dc_name,NEW.time_start,'20s');
END;
CREATE TRIGGER gl_sync_reqs_on_shader_constants_m44 AFTER INSERT ON shader_constants_m44
BEGIN
	INSERT OR IGNORE INTO gl_sync_reqs_named VALUES (NEW.dc_name,NEW.time_start,'21cm');
END;
CREATE TRIGGER gl_sync_reqs_on_shader_constants_v4 AFTER INSERT ON shader_constants_v4
BEGIN
	INSERT OR IGNORE INTO gl_sync_reqs_named VALUES (NEW.dc_name,NEW.time_start,'21cv');
END;
CREATE TRIGGER gl_sync_reqs_on_geometry_data_relational AFTER INSERT ON geometry_data_relational
BEGIN
	INSERT OR IGNORE INTO gl_sync_reqs_named VALUES (NEW.geometry_name,NEW.time_start,'10g');
END;
/*CREATE TABLE gl_sync_req.geometry_reqs(
	geometry_name VARCHAR,
);*/
