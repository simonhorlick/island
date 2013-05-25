module Main where

import Graphics.GPipe
import Graphics.UI.GLUT( Window, mainLoop, idleCallback, getArgsAndInitialize, ($=) )

main :: IO ()
main = do
  -- Setup GLUT
  getArgsAndInitialize

  -- Create a window
  newWindow "Island"
    (0:.0:.())
    (1280:.720:.())
    renderFrame
    initWindow

  -- Hand off execution to GLUT
  mainLoop

-- Called by GLUT each frame
renderFrame :: Vec2 Int -> IO (FrameBuffer RGBFormat () ())
renderFrame size = do
  return $ newFrameBufferColor (RGB (0.1:.0.3:.0.6:.()))

-- Set up GLUT callbacks
initWindow :: Window -> IO ()
initWindow win = idleCallback $= Nothing

