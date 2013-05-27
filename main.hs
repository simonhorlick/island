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
    renderFrame
    initWindow

  -- Hand off execution to GLUT
  mainLoop

-- Called by GLUT each frame
renderFrame :: Vec2 Int -> IO (FrameBuffer RGBFormat () ())
renderFrame size = do
  return $ draw (rasteriseTriangle size) clear
  where
    draw  = paintColor NoBlending (RGB $ Vec.vec True)  
    clear = newFrameBufferColor (RGB (Vec.fromList [0.1,0.3,0.6]))

-- Set up GLUT callbacks
initWindow :: Window -> IO ()
initWindow win = idleCallback $= Nothing

-- Shader vertex types
type Position = Vec3 (Vertex Float)
type Position' = Vec4 (Vertex Float) -- homogenised position
type Normal = Vec3 (Vertex Float)

-- This implements the fragment shader
 
rasteriseTriangle :: Vec2 Int -> FragmentStream (Color RGBFormat (Fragment Float))
rasteriseTriangle size = fmap (\(front,color) -> RGB color) $ rasterizeFrontAndBack $ cube_proc_scene


-- This implements the vertex shader

cube_proc_scene :: PrimitiveStream Triangle (Position', Normal)
cube_proc_scene = fmap (cube_transform) $ gridStream

cube_transform :: (Position, Normal) -> (Position', Normal)
cube_transform (pos,norm) = (transformedPos,norm)
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

