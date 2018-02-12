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

wsize :: Display.WindowSize x y
wsize = Display.WindowSize 400 400

main :: IO ()
main = do
    Display.init wsize
    win <- Glut.createWindow title
    putStrLn $ "Window name: " ++ (show win)
    Glut.displayCallback Glut.$= Display.update 
    Glut.keyboardMouseCallback Glut.$= Just Keyboard.handleKeyboardMouse
    Glut.mainLoop

