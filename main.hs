module Main where

import Graphics.GPipe
import Graphics.UI.GLUT( Window, mainLoop, idleCallback, postRedisplay, getArgsAndInitialize, ($=) )

import Drawing

main :: IO ()
main = do
  -- Setup GLUT
  getArgsAndInitialize

  -- Create a window
  newWindow "Island"
    (0:.0:.())
    (1280:.720:.())
    render
    initWindow

  -- Hand off execution to GLUT
  mainLoop

-- Set up GLUT callbacks
initWindow :: Window -> IO ()
initWindow win = idleCallback $= Just (postRedisplay (Just win))

