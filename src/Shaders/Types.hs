module Shaders.Types where

import qualified Graphics.Rendering.OpenGL as OpenGL

data ShaderInfo a b = ShaderInfo {
    filepath :: FilePath,
    stype :: OpenGL.ShaderType
} deriving (Show)

data ShaderStatus
    = LinkNotOK
    | LinkOK
    | ProgramNotValid
    | CannotReadShaderFile
    | ValidateStatusNotOK
    | ValidateStatusOK
    deriving (Show)
