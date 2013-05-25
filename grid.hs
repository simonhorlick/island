module Grid  
( gridTriStripIndices  
  ) where 

-- Zip the lists a and b, concatenating into a single list
flatZip a b = concat $ zipWith (\x y -> [x,y]) a b

-- Grids are drawn as OpenGL triangle strips. The vertices are traversed in the
-- order v00 v10 v01 v11. The indices into the vertex buffer then become 0, w,
-- 1, (w+1), 2, (w+2), ..., (w-1), (2*w-1). At the end of a row we must draw
-- some degenerate triangles to keep the winding order consistent.
gridRowTriStripIndices w =
    flatZip [0..w-1] [w..(2*w-1)]
    ++ [2*w-1,2*w-1] -- fix winding order
    ++ flatZip [3*w-1,3*w-2..2*w+1] [2*w-2,2*w-3..w]
    ++ [2*w,2*w,2*w] -- fix winding order again

gridTriStripIndices w h = gridRowTriStripIndices w

