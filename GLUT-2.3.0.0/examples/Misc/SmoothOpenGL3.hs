{-
   SmoothOpenGL3.hs (adapted from freeglut's smooth_opengl3.c example)
   Copyright (c) Sven Panne 2009 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file LICENSE
-}

import Control.Monad
import Data.List
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.UI.GLUT
import System.Exit
import System.IO

data State = State {
   vertexBufferName :: BufferObject,
   fgProjectionMatrixIndex :: UniformLocation,
   fgColorIndex :: AttribLocation ,
   fgVertexIndex :: AttribLocation,
   projectionMatrix :: GLmatrix GLfloat }

checkError :: String -> IO ()
checkError functionName = get errors >>= mapM_ reportError
   where reportError e = 
            hPutStrLn stderr (showError e ++ " detected in " ++ functionName)
         showError (Error category message) =
            "GL error" ++ show category ++ " (" ++ message ++ ")"

varray :: [GLfloat]
varray = [
   1, 0, 0, -- red
   5, 5,    -- lower left

   0, 1, 0, -- green
   25, 5,   -- lower right

   0, 0, 1, -- blue
   5, 25 ]  -- upper left

numColorComponents :: NumComponents
numColorComponents = 3

numVertexComponents :: NumComponents
numVertexComponents = 2

sizeOfComponent :: Int
sizeOfComponent = sizeOf (head varray)

stride :: Stride
stride = fromIntegral sizeOfComponent * fromIntegral (numColorComponents + numVertexComponents)

sizeOfVarray :: Int
sizeOfVarray = length varray * sizeOfComponent

numElements :: NumArrayIndices
numElements = fromIntegral sizeOfVarray `div` fromIntegral stride

initBuffer :: IO BufferObject
initBuffer = do
   [bufferObject] <- genObjectNames 1
   bindBuffer ArrayBuffer $= Just bufferObject
   withArray varray $ \buffer ->
      bufferData ArrayBuffer $= (fromIntegral sizeOfVarray, buffer, StaticDraw)
   checkError "initBuffer"
   return bufferObject

vertexShaderSource :: String
vertexShaderSource = unlines [
   "#version 140",
   "uniform mat4 fg_ProjectionMatrix;",
   "in vec4 fg_Color;",
   "in vec4 fg_Vertex;",
   "smooth out vec4 fg_SmoothColor;",
   "void main()",
   "{",
   "   fg_SmoothColor = fg_Color;",
   "   gl_Position = fg_ProjectionMatrix * fg_Vertex;",
   "}" ]

fragmentShaderSource :: String
fragmentShaderSource = unlines [
   "#version 140",
   "smooth in vec4 fg_SmoothColor;",
   "out vec4 fg_FragColor;",
   "void main(void)",
   "{",
   "   fg_FragColor = fg_SmoothColor;",
   "}" ]

checked :: (t -> IO ()) -> (t -> GettableStateVar Bool) -> (t -> GettableStateVar String) -> String -> t -> IO ()
checked action getStatus getInfoLog message object = do
   action object
   status <- get (getStatus object)
   unless status $
      hPutStrLn stderr . ((message ++ " log: ") ++) =<< get (getInfoLog object)

compileAndCheck :: Shader s => s -> IO ()
compileAndCheck = checked compileShader compileStatus shaderInfoLog "compile"

compileShaderSource :: Shader s => [String] -> IO s
compileShaderSource source = do
   [shader] <- genObjectNames 1
   shaderSource shader $= source
   compileAndCheck shader
   return shader

linkAndCheck :: Program -> IO ()
linkAndCheck = checked linkProgram linkStatus programInfoLog "link"

createProgram :: [VertexShader] -> [FragmentShader] -> IO Program
createProgram vertexShaders fragmentShaders = do
   [program] <- genObjectNames 1
   attachedShaders program $= (vertexShaders, fragmentShaders)
   linkAndCheck program
   return program

initShader :: IO (UniformLocation, AttribLocation, AttribLocation)
initShader = do
   vertexShader <- compileShaderSource [vertexShaderSource]
   fragmentShader <- compileShaderSource [fragmentShaderSource]
   program <- createProgram [vertexShader] [fragmentShader]
   currentProgram $= Just program

   projectionMatrixIndex <- get (uniformLocation program "fg_ProjectionMatrix")

   colorIndex <- get (attribLocation program "fg_Color")
   vertexAttribArray colorIndex $= Enabled

   vertexIndex <- get (attribLocation program "fg_Vertex")
   vertexAttribArray vertexIndex $= Enabled

   checkError "initShader"
   return (projectionMatrixIndex, colorIndex, vertexIndex)

initRendering :: IO ()
initRendering = do
   clearColor $= Color4 0 0 0 0
   checkError "initRendering"

myInit :: IO State
myInit = do
   bufferObject <- initBuffer
   (projectionMatrixIndex, colorIndex, vertexIndex) <- initShader
   initRendering
   m <- newMatrix ColumnMajor (replicate 16 0)
   return $ State {
      vertexBufferName = bufferObject,
      fgProjectionMatrixIndex = projectionMatrixIndex,
      fgColorIndex = colorIndex,
      fgVertexIndex = vertexIndex,
      projectionMatrix = m }

dumpInfo :: IO ()
dumpInfo = do
   let dump message var = putStrLn . ((message ++ ": ") ++) =<< get var
   dump "Vendor" vendor
   dump "Renderer" renderer
   dump "Version" glVersion
   dump "GLSL" shadingLanguageVersion
   checkError "dumpInfo"

bufferObjectPtr :: Integral a => a -> Ptr b
bufferObjectPtr = plusPtr (nullPtr :: Ptr GLchar) . fromIntegral

vertexArrayDescriptor :: NumComponents -> NumComponents -> VertexArrayDescriptor a
vertexArrayDescriptor count offset =
   VertexArrayDescriptor count Float stride (bufferObjectPtr (fromIntegral sizeOfComponent * offset))

triangle :: State -> IO ()
triangle state = do
   withMatrix (projectionMatrix state) $ \order buffer ->
      uniformMatrix4fv (fgProjectionMatrixIndex state) 1 (order == RowMajor) buffer
   bindBuffer ArrayBuffer $= Just (vertexBufferName state)
   vertexAttribPointer (fgColorIndex state) $=
      (ToFloat, vertexArrayDescriptor numColorComponents 0)
   vertexAttribPointer (fgVertexIndex state) $=
      (ToFloat, vertexArrayDescriptor numVertexComponents numColorComponents)
   drawArrays Triangles 0 numElements
   checkError "triangle"

-- The OpenGL package offers no interface for glUniformMatrix*fv yet
uniformMatrix4fv :: UniformLocation -> GLsizei -> Bool -> Ptr GLfloat -> IO ()
uniformMatrix4fv location count =
   glUniformMatrix4fv (uniformLocationToGLint location) count . marshalGLboolean
   where marshalGLboolean x = fromIntegral $ case x of
            False -> gl_FALSE
            True -> gl_TRUE
         -- MEGA HACK because UniformLocation is abstract
         uniformLocationToGLint = read . head . tail . words . show

display :: State -> DisplayCallback
display state = do
   clear [ ColorBuffer ]
   triangle state
   flush
   checkError "display"

loadOrtho :: (Matrix m, MatrixComponent a, Fractional a) => m a -> a -> a -> a -> a -> a -> a -> IO ()
loadOrtho m l r b t n f =
   fillMatrix m [
      [2 / (r - l),
       0,
       0,
       0],

      [0,
       2 / (t - b),
       0,
       0],

      [0,
       0,
       -2 / (f - n),
       0],

      [-(r + l) / (r - l),
       -(t + b) / (t - b),
       -(f + n) / (f - n),
       1 ]]

fillMatrix :: (Matrix m, MatrixComponent a) => m a -> [[a]] -> IO ()
fillMatrix m xs =
   withMatrix m $ \order buffer ->
      pokeArray buffer . concat . rearrange order $ xs

rearrange :: MatrixOrder -> [[a]] -> [[a]]
rearrange ColumnMajor = id
rearrange RowMajor = transpose

loadOrtho2D :: (Matrix m, MatrixComponent a, Fractional a) => m a -> a -> a -> a -> a -> IO ()
loadOrtho2D m l r b t = loadOrtho m l r b t (-1) 1

reshape :: State -> ReshapeCallback
reshape state size@(Size w h) = do
   viewport $= (Position 0 0, size)
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then loadOrtho2D (projectionMatrix state) 0 30 0 (30 * hf / wf)
      else loadOrtho2D (projectionMatrix state) 0 (30 * wf / hf) 0 30
   checkError "reshape"

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

main :: IO ()
main = do
   (progName, args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   -- add command line argument "classic" for a pre-3.x context
   unless (args == ["classic"]) $ do
      initialContextVersion $= (3, 1)
      initialContextFlags $= [ ForwardCompatibleContext, DebugContext ]
   initialWindowSize $= Size 500 500
   initialWindowPosition $= Position 100 100
   createWindow progName
   dumpInfo
   state <- myInit
   displayCallback $= display state
   reshapeCallback $= Just (reshape state)
   keyboardMouseCallback $= Just keyboard
   mainLoop
