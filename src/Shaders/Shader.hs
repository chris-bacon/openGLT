module Shaders.Shader where

import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT as Glut

--OpenGL.createShader
--OpenGL.createProgram

loadShader :: FilePath -> IO String
loadShader = readFile
