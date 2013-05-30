module Drawing
( render
  ) where

import Graphics.GPipe
import qualified Data.Vec as Vec

import Grid
import ShaderTypes

-- Render a frame
render :: Texture2D RGBFormat -> Vec2 Int -> IO (FrameBuffer RGBFormat () ())
render texture size = do
  return $ draw (rasterise texture size) clear
  where
    draw  = paintColor NoBlending (RGB $ Vec.vec True)  
    clear = newFrameBufferColor (RGB (Vec.fromList [0.1,0.3,0.6]))

-- Default fragment shader
rasterise :: Texture2D RGBFormat -> Vec2 Int -> FragmentStream (Color RGBFormat (Fragment Float))
rasterise texture size = fmap (fixedLight texture) $ rasterizeFront transformedVertices
  where transformedVertices = transformVertices size

-- TODO: Vary light intensity over distance
fixedLight :: Texture2D RGBFormat -> (FragmentNormal, FragmentTexCoord) -> Color RGBFormat (Fragment Float)
fixedLight texture (norm, texcoord) = fragmentColour
  where
    fragmentColour = RGB (surfaceColour * Vec.vec lightIntensity)
    RGB surfaceColour = sample (Sampler Linear Wrap) texture texcoord
    lightIntensity = norm `dot` toGPU lightDirection
    lightDirection = (0.2:.0.5:.0.3:.())

-- Apply model view projection matrices to each vertex in the stream
transformVertices :: Vec2 Int -> PrimitiveStream Triangle (Position', (Normal, TexCoord))
transformVertices size = fmap (transformVertex size) $ gridStream

-- Default hardware transform
transformVertex :: Vec2 Int -> (Position, (Normal, TexCoord)) -> (Position', (Normal, TexCoord))
transformVertex (width:.height:.()) (pos, (norm, texcoord)) = (transformedPos, (norm, texcoord))
    where
         viewMat = (translation ((-64):.0:.(-20):.())) `multmm` (rotationX (pi/6)) `multmm` (rotationY (pi/2))
         projMat = perspective 1 100 (pi/3) aspectRatio
         aspectRatio = (fromIntegral width) / (fromIntegral height)
         viewProjMat = projMat `multmm` viewMat
         transformedPos = toGPU viewProjMat `multmv` (homPoint pos :: Vec4 (Vertex Float))

