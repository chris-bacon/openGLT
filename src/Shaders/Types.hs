module Shaders.Types where

import qualified Graphics.Rendering.OpenGL as OpenGL

data ShaderInfo filepath stype = ShaderInfo {
    filepath :: FilePath,
    stype :: OpenGL.ShaderType
} deriving (Show)

data ShaderErrors
    = LinkNotOkay
    | ProgramNotValid
    | CannotReadShaderFile 
    deriving (Show)

