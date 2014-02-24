{-# LANGUAGE ScopedTypeVariables #-}
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw
import qualified Graphics.UI.GLUT as U
import Data.Bits
import Foreign.Storable
import Foreign.Ptr
import Data.Char
import SQLUtil
import Database.SQLite3
import GHC.IO.Handle
import GHC.IO.Handle.FD
import Foreign.Marshal.Array
import qualified Data.Vec as V
import qualified Data.Vec.LinAlg.Transform3D as V
main = do
	(_,[wS,hS,deltaTS,dbS])<-U.getArgsAndInitialize
	--let [w,h,deltaT] = map (read::String->GLsizei) args
	U.initialWindowSize $= U.Size (read wS) (read hS)
	U.initialDisplayMode $= [U.RGBAMode,U.WithAlphaComponent,U.WithDepthBuffer,U.DoubleBuffered]
	wnd<-U.createWindow "Player"
	db<-open dbS
	U.displayCallback $= present (fromIntegral (read deltaTS))
	[fs]::[FragmentShader]<-genObjectNames 1
	shaderSource fs $= ["varying vec3 c; void main(){ gl_FragColor = vec4(c, 1.0); }"]
	compileShader fs
	get (compileStatus fs) >>= print
	get (shaderInfoLog fs) >>= putStrLn
	[vs]::[VertexShader]<-genObjectNames 1
	shaderSource vs $= ["\
\ varying vec3 c; \
\ uniform float s;\
\ uniform mat4 mvp;\
\ void main(void){\
\  vec4 a = gl_Vertex; \
\  c=(a.xyz); \
\  gl_Position = mvp * a;}"]
	compileShader vs
	get (compileStatus vs) >>= print
	get (shaderInfoLog vs) >>= putStrLn
	[p]::[Program]<-genObjectNames 1
	attachedShaders p$=([vs],[fs])
	linkProgram p
	get (linkStatus p) >>= print
	validateProgram p
	get (validateStatus p) >>= print
	currentProgram $= Just p
	get (uniformLocation p "s")>>= (\l->uniform l$=FogCoord1 (0.2::GLfloat))
	let ls=V.matToList (testMVP::V.Mat44 GLfloat)
	print ls
	hFlush stdout
	i<-newArray ls
	get (uniformLocation p "mvp")>>= (\l->glUniformMatrix4fv (uniformVarID l) 1 (fromIntegral gl_TRUE) i )
	print (vertexShaderID vs)
	depthFunc$=Just Lequal
	get depthFunc>>=print
	hFlush stdout
	U.mainLoop
	where
	present t = do
		U.addTimerCallback t (present t)
		glClearColor 1 0 0 0
		clear [ColorBuffer,DepthBuffer]
		U.renderObject U.Solid (U.Teapot 0.5)
		--glFlush
		U.swapBuffers
		--print 1>>hFlush stdout
	testMVP = combMats [p,v]
		where
		combMats = foldl1 V.multmm
		p::V.Mat44 GLfloat = 
			V.perspective 0.1 50 (pi/4) (16/9)
		v::V.Mat44 GLfloat = 
			lookAt (V.fromList [0,0,1]) (V.fromList [10,10,10]) (V.fromList [0,0,1])
		lookAt up eye target = combMats [V.transpose (V.rotationLookAt up eye target),V.translation (-eye)]
		m::V.Mat44 GLfloat = V.identity --translation $ V.fromList [0,0,-10] :: V.Mat44 GLfloat --V.matFromList [0.2,0,0,1, 0,0.2,0,0, 0,0,0.2,0, 0,0,0,1] --

		--postRedisplay Nothing
--	reshapeCallback $= Just (\s->return ())
--	windowStateCallback $= Just (\s->return ())
--	closeCallback $= Just (return ())
--	keyboardMouseCallback $= Just (\k ks m p->return ())
--	mainLoop
