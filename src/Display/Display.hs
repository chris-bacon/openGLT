module Display.Display where

import Display.Types
import Shaders.Shader

import qualified Graphics.UI.GLUT as Glut

update :: Glut.DisplayCallback
update = do
    Glut.clear [Glut.ColorBuffer]
    Glut.swapBuffers

init :: WindowSize x y -> IO ()
init (WindowSize x y) = do
    (progName, _args) <- Glut.getArgsAndInitialize
    putStrLn $ "Program name: " ++ progName
    putStrLn $ "Window size: " ++ (show x) ++ "x" ++ (show y)
    Glut.initialWindowSize Glut.$= (Glut.Size x y)
    Glut.initialDisplayMode Glut.$= [Glut.DoubleBuffered]

