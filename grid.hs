module Grid
( gridTriStripIndices
, gridStream
  ) where

import Graphics.GPipe hiding (row)
import Control.Applicative

import ShaderTypes

-- Zip the lists a and b, concatenating into a single list
flatZip a b = concat $ zipWith (\x y -> [x,y]) a b

-- Grids are drawn as OpenGL triangle strips. The vertices are traversed in the
-- order v00 v10 v01 v11. The indices into the vertex buffer then become 0, w,
-- 1, (w+1), 2, (w+2), ..., (w-1), (2*w-1). At the end of a row we must draw
-- some degenerate triangles to keep the winding order consistent.
forwardRow :: Int -> [Int]
forwardRow w =
  flatZip [0..w-1] [w..(2*w-1)]
  ++ [2*w-1,2*w-1] -- fix winding order

backwardRow :: Int -> [Int]
backwardRow w = 
  flatZip [3*w-1,3*w-2..2*w+1] [2*w-2,2*w-3..w]
  ++ [2*w,2*w] -- fix winding order again

row :: Int -> Int -> [Int]
row x w = if even x then forwardRow w
                    else backwardRow w

gridTriStripIndices w h =
  concat [ ((+(x `quot` 2)*2*w) <$> row x w) | x<-[0..h-1] ]

height :: Float -> Float -> Float
height x z = sin (pi*z)

-- Create a triangle stream describing a tesselated grid
gridStream :: PrimitiveStream Triangle (Position, (Normal, TexCoord))
gridStream = toIndexedGPUStream TriangleStrip vertices indices
  where
    vertices = [ (x:.(height x z):.z:.(), (0.0:.1.0:.0.0:.(), x:.z:.())) |
      x<-[0.0,(1.0/(fromIntegral w))..1.0],
      z<-[0.0,(1.0/(fromIntegral h))..1.0] ]
    indices = gridTriStripIndices w h
    w = h 
    h = 8

