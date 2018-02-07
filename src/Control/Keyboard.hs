module Control.Keyboard where

import qualified Graphics.UI.GLUT as Glut
import System.Exit (exitWith, ExitCode(ExitSuccess))

handleKeyboardMouse :: Glut.Key -> Glut.KeyState -> Glut.Modifiers -> Glut.Position -> IO () 
handleKeyboardMouse (Glut.Char '\27') Glut.Down _ _ = handleEscapeKey
handleKeyboardMouse _ _ _ _ = return ()

-- Build and handle keyboard and mouse callback handlers here
handleEscapeKey :: IO () 
handleEscapeKey = exitWith ExitSuccess

