INSERT INTO resources VALUES
	('g','testQUAD'),
	('s','testVS'),
	('s','testFS');
INSERT INTO draw_calls VALUES 
	('testDC',0,33),
	('testDC',66,99),
	('testDC1',0,100);
INSERT INTO draw_call_prop VALUES
	('testDC',0,'ts','testQUAD',0),
	('testDC1',0,'ll','testQUAD',0);
INSERT INTO shader_binding VALUES
	('testDC',0,'testVS','','testFS'),
	('testDC1',0,'testVS','','testFS');
INSERT INTO shader_constants_m44 VALUES
	('testDC',0,'pvm',
	1,0,0,-0.3,
	0,1,0,0,
	0,0,1,0,
	0,0,0,1),
	('testDC1',0,'pvm',
	1,0,0,0.5,
	0,1,0,0,
	0,0,1,0,
	0,0,0,1);
INSERT INTO shader_constants_v4 VALUES
	('testDC',0,'color',0.5,0.5,0.5,1),
	('testDC',15,'color',0.5,0,0.5,1),
	('testDC',30,'color',0.5,0.5,0,1),
	('testDC1',0,'color',1,0,0,1),
	('testDC1',20,'color',1,0,1,1),
	('testDC1',40,'color',1,1,1,1),
	('testDC1',60,'color',1,1,0,1),
	('testDC1',80,'color',0,1,0,1);
INSERT INTO geometry_type_attrib_def VALUES
	('simplestGeometry','pos','2f',3,0),
	('simplestGeometry','color_mod','2f',1,0);
INSERT INTO geometry_typing VALUES
	('testQUAD','simplestGeometry');
INSERT INTO vertex_shader_typing VALUES
	('testVS','simplestGeometry');
/*INSERT INTO geometry_data_attrib_source VALUES
	('testQUAD','pos',0,'r');*/
INSERT INTO geometry_data_relational VALUES
	('testQUAD','pos',0,4),
	('testQUAD','pos',50,4),
	('testQUAD','color_mod',0,4);
INSERT INTO geometry_data VALUES
	('testQUAD','color_mod',0,0,0.1),
	('testQUAD','color_mod',0,1,0.3),
	('testQUAD','color_mod',0,2,0.7),
	('testQUAD','color_mod',0,3,1.0),
	('testQUAD','pos',0,0,-0.09),
	('testQUAD','pos',0,1,-0.16),
	('testQUAD','pos',0,2,-0.5),
	('testQUAD','pos',0,3,-0.09),
	('testQUAD','pos',0,4, 0.16),
	('testQUAD','pos',0,5,-0.5),
	('testQUAD','pos',0,6, 0.09),
	('testQUAD','pos',0,7,-0.16),
	('testQUAD','pos',0,8,-0.5),
	('testQUAD','pos',0,9, 0.09),
	('testQUAD','pos',0,10,0.16),
	('testQUAD','pos',0,11,-0.5),
	('testQUAD','pos',50,0,-0.009),
	('testQUAD','pos',50,1,-0.016),
	('testQUAD','pos',50,2,-0.05),
	('testQUAD','pos',50,3,-0.009),
	('testQUAD','pos',50,4, 0.016),
	('testQUAD','pos',50,5,-0.05),
	('testQUAD','pos',50,6, 0.009),
	('testQUAD','pos',50,7,-0.016),
	('testQUAD','pos',50,8,-0.05),
	('testQUAD','pos',50,9, 0.009),
	('testQUAD','pos',50,10,0.016),
	('testQUAD','pos',50,11,-0.05);
INSERT INTO text_res VALUES
	('s','testVS',
'
uniform mat4 pvm;
attribute vec4 pos;
attribute float color_mod;
varying float cm;
void main(){
	gl_Position = pvm*pos;
	cm = color_mod;
	}
'
	),
	('s','testFS',
'
varying float cm;
uniform vec4 color;
void main(){
	gl_FragColor=color*cm;
	}
'
	);

/**/
