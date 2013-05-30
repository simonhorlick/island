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

-- Round down to nearest even number
round2 :: Int -> Int
round2 x = (x `quot` 2) * 2

gridTriStripIndices w h =
  concat [ ((+(round2 x)*(w+1)) <$> row x (w+1)) | x<-[0..h-1] ]

height :: Float -> Float -> Float
height x z = sin(x/10) + cos(z/10)

-- Create a triangle stream describing a tesselated grid
gridStream :: PrimitiveStream Triangle (Position, (Normal, TexCoord))
gridStream = toIndexedGPUStream TriangleStrip vertices indices
  where
    vertices = [ (x:.(height x z):.z:.(), (0.0:.1.0:.0.0:.(), x:.z:.())) |
      z<-[0.0..(fromIntegral h)],
      x<-[0.0..(fromIntegral w)] ]
    indices = gridTriStripIndices w h
    w = 128 -- number of squares width
    h = 128 -- number of squares height

