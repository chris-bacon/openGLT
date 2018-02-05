module Main where

import Lib
import Keys

import Control.Monad

import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT as Glut
import System.Exit (exitWith, ExitCode(ExitSuccess))

type Title = String

title :: Title
title = "OpenGL Intro"

handleKeyboardMouse :: Glut.Key -> Glut.KeyState -> Glut.Modifiers -> Glut.Position -> IO () 
handleKeyboardMouse (Glut.Char '\27') Glut.Down _ _ = handleEscapeKey
handleKeyboardMouse (Glut.Char '\97') Glut.Down _ _ = printKey (Glut.Char '\97')
handleKeyboardMouse _ _ _ _ = return ()

handleDisplay :: Glut.DisplayCallback
handleDisplay = do
    Glut.clear [Glut.ColorBuffer]
    Glut.swapBuffers

-- Build and handle keyboard and mouse callback handlers here
handleEscapeKey :: IO () 
handleEscapeKey = exitWith ExitSuccess

printKey :: Glut.Key -> IO ()
printKey a = do
    print (show a)

main :: IO ()
main = do
--    progName <- Glut.initialize "test" [""]
    (progName, _args) <- Glut.getArgsAndInitialize
    print $ "Program name: " ++ progName
    Glut.initialWindowSize Glut.$= (Glut.Size 1080 920)
    Glut.initialDisplayMode Glut.$= [Glut.DoubleBuffered]
    win <- Glut.createWindow title
    print $ "Window name: " ++ (show win)
    Glut.displayCallback Glut.$= handleDisplay
    Glut.keyboardMouseCallback Glut.$= Just handleKeyboardMouse
    Glut.mainLoop
    print "success"
