module Display.Types where

import qualified Graphics.UI.GLUT as Glut

data WindowSize x y = WindowSize {
    x :: Glut.GLsizei,
    y :: Glut.GLsizei
} deriving (Show)
