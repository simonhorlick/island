module Texture
( loadTexture
) where

import Graphics.GPipe
import Foreign.Ptr (plusPtr)
import Data.Bitmap.Pure (withBitmap)
import Codec.Image.STB

loadTexture' comp io path = do
  image <- loadImage' path comp
  either
    (ioError . userError)
    (`withBitmap` io)
    image

texture2DFromImage cpufmt fmt path (w,h) comp 0 ptr = newTexture cpufmt fmt (w:.h:.()) [ptr]
texture2DFromImage _ _ path _ _ _ _ = ioError $ userError ("loadTexture: Row padding is not supported, in " ++ show path)

loadTexture fmt path = loadTexture' 3 (texture2DFromImage (PerComp3 UnsignedByteFormat) fmt path) path

