module Main where

import Graphics.GPipe
import Graphics.UI.GLUT( Window, mainLoop, idleCallback, postRedisplay, getArgsAndInitialize, ($=) )

import Grid
import ShaderTypes

import qualified Data.Vec as Vec
import qualified Data.Vec.LinAlg as LinAlg -- projection

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

-- Called by GLUT each frame
render :: Vec2 Int -> IO (FrameBuffer RGBFormat () ())
render size = do
  return $ draw (rasterise size) clear
  where
    draw  = paintColor NoBlending (RGB $ Vec.vec True)  
    clear = newFrameBufferColor (RGB (Vec.fromList [0.1,0.3,0.6]))

-- Default fragment shader
rasterise :: Vec2 Int -> FragmentStream (Color RGBFormat (Fragment Float))
rasterise size = fmap (fixedLight) $ rasterizeFront transformedVertices
  where transformedVertices = transformVertices size

-- TODO: Vary light intensity over distance
fixedLight :: Vec3 (Fragment Float) -> Color RGBFormat (Fragment Float)
fixedLight (norm) = color
  where
    li = norm `dot` toGPU lightDirection
    color = RGB (li:.li:.li:.())
    lightDirection = (0.2:.0.5:.0.3:.())

-- Apply model view projection matrices to each vertex in the stream
transformVertices :: Vec2 Int -> PrimitiveStream Triangle (Position', Normal)
transformVertices size = fmap (transformVertex size) $ gridStream

-- Default hardware transform
transformVertex :: Vec2 Int -> (Position, Normal) -> (Position', Normal)
transformVertex (width:.height:.()) (pos,norm) = (transformedPos,norm)
    where
         viewMat = (translation (0:.0:.(-2):.())) `multmm` (rotationX (pi/6)) `multmm` (rotationY (pi/4))
         projMat = perspective 1 100 (pi/3) aspectRatio
         aspectRatio = (fromIntegral width) / (fromIntegral height)
         viewProjMat = projMat `multmm` viewMat
         transformedPos = toGPU viewProjMat `multmv` (homPoint pos :: Vec4 (Vertex Float))

