module Shaders.Shader where

import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT as Glut

shaderTypes :: [OpenGL.ShaderType]
shaderTypes = [OpenGL.VertexShader, OpenGL.FragmentShader]

-- glCreateProgram creates an empty program object and returns a non-zero value by which it can be referenced. A program object 
-- is an object to which shader objects can be attached. This provides a mechanism to specify the shader objects that will be 
-- linked to create a program
createProgram :: IO OpenGL.Program
createProgram = OpenGL.createProgram

-- glCreateShader creates an empty shader object and returns a non-zero value by which it can be referenced. 
-- glShaderSource sets the source code in shader to the source code in the array of strings specified by string. 
-- Any source code previously stored in the shader object is completely replaced.
-- glCompileShader compiles the source code strings that have been stored in the shader object.
--createShader :: OpenGL.ShaderType -> String -> IO Shader
createShader shaderType shaderText = do
    shader <- OpenGL.createShader shaderType
    OpenGL.shaderSourceBS shader Glut.$= shaderText -- glShaderSource
    OpenGL.compileShader shader -- glCompileShader
    return shader

-- In order to create a complete shader program, there must be a way to specify the list of things that will be linked together. 
-- Program objects provide this mechanism. Shaders that are to be linked together in a program object must first be attached to
-- that program object. glAttachShader attaches the shader object specified by shader to the program object specified by program. 
-- This indicates that shader will be included in link operations that will be performed on program.
attachShader :: OpenGL.Program -> OpenGL.Shader -> IO ()
attachShader p s = OpenGL.attachShader p s



--attribLocation :: OpenGL.Program -> String
attribLocation program string = OpenGL.attribLocation program string $= OpenGL.AttribLocation 0




-- glLinkProgram links the program object specified by program. If any shader objects of type GL_VERTEX_SHADER are attached to
-- program, they will be used to create an executable that will run on the programmable vertex processor. If any shader objects of 
-- type GL_GEOMETRY_SHADER are attached to program, they will be used to create an executable that will run on the programmable 
-- geometry processor. If any shader objects of type GL_FRAGMENT_SHADER are attached to program, they will be used to create an 
-- executable that will run on the programmable fragment processor.
OpenGL.linkProgram :: Program -> IO ()


linkOk <- OpenGL.get $ OpenGL.linkStatus program



-- glValidateProgram checks to see whether the executables contained in program can execute given the current OpenGL state
-- https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glValidateProgram.xhtml
OpenGL.validateProgram program

OpenGL.currentProgram $= Just program -- is this glUseProgram?

-- ✓ createProgram
-- ✓ createShader
-- ✓ attachShader
-- attribLocation
-- linkProgram
-- validateProgram

-- m_program = glCreateProgram():
-- m_shaders[0] = CreateShader(LoadShader(fileName + ".vs"), GL_VERTEX_SHADER)
-- m_shaders[1] = CreateShader(LoadShader(fileName + ".fs"), GL_FRAGMENT_SHADER)
--
-- for (unsigned int = 0; i < NUM_SHADERS; i++)
--     glAttachShader(m_program, m_shaders[i];
--
-- glBindAttribLocation(m_program, 0, "position");  -- binds something called position in the vertex shader
--
-- glLinkProgram(m_program);
--
-- glValidateProgram(m_program);


-- Bind()
-- {
--    glUseProgram(m_program)
-- }

-- CreateShader(text, shaderType)
-- {
--    GLuint shader = glCreateShader(shaderType);
--    
--    if (shader == 0)
--        std::ceer << "Error: shader creation failed" << std::endl;
--
--    shaderSourceStrings[0] = text.c_str();
--    shaderSourceStringsLengths[0] = text.length();
--
--    glShaderSource(shader, 1, shaderSourceStrings, sourceStringsLengths) -- send code to gpu
--    glCompileShader(shader);
--
--    return shader;
-- }

-- TODO: Write exception handling for this - when FilePath is not found
loadShader :: FilePath -> IO String
loadShader file = readFile file
