module ShaderTypes where

import Graphics.GPipe
--import qualified Data.Vec as Vec

type Position = Vec3 (Vertex Float)
type Position' = Vec4 (Vertex Float) -- homogenised position
type Normal = Vec3 (Vertex Float)

