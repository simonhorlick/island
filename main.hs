module Main where

import Graphics.GPipe
import Graphics.UI.GLUT( Window, mainLoop, idleCallback, getArgsAndInitialize, ($=) )

import Grid

import qualified Data.Vec as Vec
import Data.Vec.LinAlg -- projection

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


-- This implements the fragment shader
 
rasteriseTriangle :: Vec2 Int -> FragmentStream (Color RGBFormat (Fragment Float))
rasteriseTriangle size = fmap (\(front,color) -> RGB color) $ rasterizeFrontAndBack $ procTriangle size 


-- This implements the vertex shader

procTriangle :: Vec2 Int -> PrimitiveStream Triangle (Vec4 (Vertex Float), Vec3 (Vertex Float))
procTriangle size = fmap (projTriangle size) gridStream 

-- Compute the projection matrix for the triangle
projTriangle :: Vec2 Int -> Vec3 (Vertex Float) -> (Vec4 (Vertex Float), Vec3 (Vertex Float))
projTriangle size pos = (modelViewProjection `multmv` homPos, colour)
  where homPos = homPoint pos :: Vec4 (Vertex Float)  
        colour = toGPU $ 0:.1:.0:.()
        modelViewProjection = model `multmm` (view `multmm` proj)
        proj = toGPU (perspective 1 20 (pi/3) (fromIntegral 1280 / fromIntegral 720))
        view = identity
        model = identity

gridVertices = 
  [
    0.0:.0.0:.0.0:.(),
    0.1:.0.0:.0.0:.(),
    0.2:.0.0:.0.0:.(),
    0.3:.0.0:.0.0:.(),
    0.0:.0.1:.0.0:.(),
    0.1:.0.1:.0.0:.(),
    0.2:.0.1:.0.0:.(),
    0.3:.0.1:.0.0:.(),
    0.0:.0.2:.0.0:.(),
    0.1:.0.2:.0.0:.(),
    0.2:.0.2:.0.0:.(),
    0.3:.0.2:.0.0:.(),
    0.0:.0.3:.0.0:.(),
    0.1:.0.3:.0.0:.(),
    0.2:.0.3:.0.0:.(),
    0.3:.0.3:.0.0:.()
  ]

-- Create a triangle stream describing a tesselated grid
gridStream :: PrimitiveStream Triangle (Vec3 (Vertex Float))
gridStream = toIndexedGPUStream TriangleStrip gridVertices (gridTriStripIndices 4 4)

