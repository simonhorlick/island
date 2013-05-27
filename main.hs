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
rasterise size = fmap (\(front, color) -> RGB color) $ rasterizeFrontAndBack (transformVertices size)

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

height :: Float -> Float -> Float
height x z = sin (pi*z)

-- Create a triangle stream describing a tesselated grid
gridStream :: PrimitiveStream Triangle (Position, Normal)
gridStream = toIndexedGPUStream TriangleStrip vertices indices
  where
    vertices = [ (x:.(height x z):.z:.(),0.0:.1.0:.0.0:.()) | x<-[0.0,(1.0/(fromIntegral w))..1.0], z<-[0.0,(1.0/(fromIntegral h))..1.0] ]
    indices = gridTriStripIndices w h
    w = h 
    h = 8

