module Main where

import Graphics.GPipe
import Graphics.UI.GLUT( Window, mainLoop, idleCallback, getArgsAndInitialize, ($=) )

import Grid

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
initWindow win = idleCallback $= Nothing

-- Called by GLUT each frame
render :: Vec2 Int -> IO (FrameBuffer RGBFormat () ())
render size = do
  return $ draw (rasterise size) clear
  where
    draw  = paintColor NoBlending (RGB $ Vec.vec True)  
    clear = newFrameBufferColor (RGB (Vec.fromList [0.1,0.3,0.6]))

-- Shader vertex types
type Position = Vec3 (Vertex Float)
type Position' = Vec4 (Vertex Float) -- homogenised position
type Normal = Vec3 (Vertex Float)

-- This implements the fragment shader
 
rasterise :: Vec2 Int -> FragmentStream (Color RGBFormat (Fragment Float))
rasterise size = fmap (\(front,color) -> RGB color) $ rasterizeFrontAndBack $ transformVertices


-- This implements the vertex shader

transformVertices :: PrimitiveStream Triangle (Position', Normal)
transformVertices = fmap (transformVertex) $ gridStream

transformVertex :: (Position, Normal) -> (Position', Normal)
transformVertex (pos,norm) = (transformedPos,norm)
    where
         viewMat = (translation (0:.0:.(-2):.())) `multmm` (rotationX (pi/6)) `multmm` (rotationY (pi/4))
         projMat = perspective 1 100 (pi/3) (1280.0 / 720.0)
         viewProjMat = projMat `multmm` viewMat
         transformedPos = toGPU viewProjMat `multmv` (homPoint pos :: Vec4 (Vertex Float))

-- TODO: Why does zip gridVertices gridNormals fail type check, but the inline vertices below pass fine?
-- Create a triangle stream describing a tesselated grid
gridStream :: PrimitiveStream Triangle (Position, Normal)
gridStream = toIndexedGPUStream TriangleStrip vertices indices
  where
    vertices = [
      (0.0:.0.0:.0.0:.(),0.0:.1.0:.0.0:.()),
      (0.1:.0.0:.0.0:.(),0.0:.1.0:.0.0:.()),
      (0.2:.0.0:.0.0:.(),0.0:.1.0:.0.0:.()),
      (0.3:.0.0:.0.0:.(),0.0:.1.0:.0.0:.()),
      (0.0:.0.0:.0.1:.(),0.0:.1.0:.0.0:.()),
      (0.1:.0.0:.0.1:.(),0.0:.1.0:.0.0:.()),
      (0.2:.0.0:.0.1:.(),0.0:.1.0:.0.0:.()),
      (0.3:.0.0:.0.1:.(),0.0:.1.0:.0.0:.()),
      (0.0:.0.0:.0.2:.(),0.0:.1.0:.0.0:.()),
      (0.1:.0.0:.0.2:.(),0.0:.1.0:.0.0:.()),
      (0.2:.0.0:.0.2:.(),0.0:.1.0:.0.0:.()),
      (0.3:.0.0:.0.2:.(),0.0:.1.0:.0.0:.()),
      (0.0:.0.0:.0.3:.(),0.0:.1.0:.0.0:.()),
      (0.1:.0.0:.0.3:.(),0.0:.1.0:.0.0:.()),
      (0.2:.0.0:.0.3:.(),0.0:.1.0:.0.0:.()),
      (0.3:.0.0:.0.3:.(),0.0:.1.0:.0.0:.()) ]
    indices = gridTriStripIndices 4 4

