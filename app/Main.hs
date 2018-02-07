module Main where

import qualified Display as Display
import qualified Shaders.Shader as Shader
import qualified Control.Keyboard as Keyboard

import Control.Monad
import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT as Glut

type Title = String

vertexShaderPath :: FilePath
vertexShaderPath = "shaders/basicShader.vs" -- hardcoded for now

fragmentShaderPath :: FilePath
fragmentShaderPath = "shaders/basicShader.fs" -- hardcoded for now

title :: Title
title = "OpenGL Intro"

main :: IO ()
main = do
    Display.init
    win <- Glut.createWindow title
    print $ "Window name: " ++ (show win)
    Glut.displayCallback Glut.$= Display.update 
    Glut.keyboardMouseCallback Glut.$= Just Keyboard.handleKeyboardMouse
    Glut.mainLoop
    print "success"

