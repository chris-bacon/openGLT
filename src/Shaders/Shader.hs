module Shaders.Shader where

-- ✓ createProgram
-- ✓ createShader
-- ✓ attachShader
-- ✓ attribLocation
-- ✓ linkProgram
-- ✓ validateProgram

import Shaders.Types

import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT as Glut
--import Data.ByteString hiding (readFile)

shaderTypes :: [OpenGL.ShaderType]
shaderTypes = [OpenGL.VertexShader, OpenGL.FragmentShader]

-- TODO do this for multiple shaderInfos
--initShaderProgram :: [ShaderInfo] -> 
initShaderProgram shaderInfo = do
    program <- createProgram
    shaderContent <- loadShader $ filepath shaderInfo
    --shader <- makeShader (stype shaderInfo) (shaderContent :: ByteString)
    --attachShader program shader
    --linkProgram program
    --checkProgram program
    print program
    print shaderContent
--    print shader

-- glCreateProgram creates an empty program object and returns a non-zero value by which it can be referenced. A program object 
-- is an object to which shader objects can be attached. This provides a mechanism to specify the shader objects that will be 
-- linked to create a program
createProgram :: IO OpenGL.Program
createProgram = OpenGL.createProgram

-- glCreateShader creates an empty shader object and returns a non-zero value by which it can be referenced. 
-- glShaderSource sets the source code in shader to the source code in the array of strings specified by string. 
-- Any source code previously stored in the shader object is completely replaced.
-- glCompileShader compiles the source code strings that have been stored in the shader object.
--makeShader :: OpenGL.ShaderType -> String -> IO OpenGL.Shader
makeShader shaderType shaderText = do
    shader <- OpenGL.createShader shaderType
    OpenGL.shaderSourceBS shader Glut.$= shaderText
    OpenGL.compileShader shader
    return shader

-- In order to create a complete shader program, there must be a way to specify the list of things that will be linked together. 
-- Program objects provide this mechanism. Shaders that are to be linked together in a program object must first be attached to
-- that program object. glAttachShader attaches the shader object specified by shader to the program object specified by program. 
-- This indicates that shader will be included in link operations that will be performed on program.
attachShader :: OpenGL.Program -> OpenGL.Shader -> IO ()
attachShader p s = OpenGL.attachShader p s

-- glBindAttribLocation is used to associate a user-defined attribute variable in the program object specified by program with a
-- generic vertex attribute index. The name of the user-defined attribute variable is passed as a null terminated string in name. 
-- The generic vertex attribute index to be bound to this variable is specified by index. When program is made part of current 
-- state, values provided via the generic vertex attribute index will modify the value of the user-defined attribute variable 
-- specified by name.
-- Attribute bindings do not go into effect until glLinkProgram is called. After a program object has been linked successfully, 
-- the index values for generic attributes remain fixed (and their values can be queried) until the next link command occurs.
--attribLocation :: OpenGL.Program -> String
attribLocation program string = OpenGL.attribLocation program string Glut.$= OpenGL.AttribLocation 0

-- glLinkProgram links the program object specified by program. If any shader objects of type GL_VERTEX_SHADER are attached to
-- program, they will be used to create an executable that will run on the programmable vertex processor. If any shader objects of 
-- type GL_GEOMETRY_SHADER are attached to program, they will be used to create an executable that will run on the programmable 
-- geometry processor. If any shader objects of type GL_FRAGMENT_SHADER are attached to program, they will be used to create an 
-- executable that will run on the programmable fragment processor.
linkProgram :: OpenGL.Program -> IO ()
linkProgram = OpenGL.linkProgram

-- glValidateProgram checks to see whether the executables contained in program can execute given the current OpenGL state
-- https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glValidateProgram.xhtml
checkProgram :: OpenGL.Program -> IO ()
checkProgram program = do
    linkOk <- OpenGL.get $ OpenGL.linkStatus program
    OpenGL.validateProgram program

useProgram program = OpenGL.currentProgram Glut.$= Just program -- is this glUseProgram?

-- TODO: Write exception handling for this - when FilePath is not found
loadShader :: FilePath -> IO String
loadShader file = readFile file

