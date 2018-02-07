module Display where

import qualified Graphics.UI.GLUT as Glut

update :: Glut.DisplayCallback
update = do
    Glut.clear [Glut.ColorBuffer]
    Glut.swapBuffers

init :: IO ()
init = do
    --    progName <- Glut.initialize "test" [""]
    (progName, _args) <- Glut.getArgsAndInitialize
    print $ "Program name: " ++ progName
    Glut.initialWindowSize Glut.$= (Glut.Size 400 400)
    Glut.initialDisplayMode Glut.$= [Glut.DoubleBuffered]

