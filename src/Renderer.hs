
module Main where

import Reader

import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Codec.Picture( PixelRGBA8(..), writePng, Image )
import qualified Data.Vector.Unboxed as V
import Control.Applicative

black = PixelRGBA8 0 0 0 255
blue = PixelRGBA8 0 0 255 80
green = PixelRGBA8 0 255 0 80

renderJourney :: (Float,Float) -> Float -> V.Vector (Int,Int) -> Drawing px ()
renderJourney (x,y) scale v =
  let scaled = V.map (\(a,b) -> (x + (fromIntegral a/scale), y + (fromIntegral b/scale))) v
      v2s = map (\(a,b) -> (V2 a b)) $ V.toList scaled
      v2s' = tail v2s
      primitives = LinePrim <$> zipWith Line v2s v2s'
  in stroke 0.5 JoinRound (CapRound, CapRound) primitives

main = do
  journeys <- mapM (getJourney "3000") [1..200]
  journeys' <- mapM (getJourney "2000") [1..200] 
  let scales = map (\j -> max (V.maximum (V.map fst j)) (V.maximum (V.map snd j))) journeys
      scales' = map (\j -> max (V.maximum (V.map fst j)) (V.maximum (V.map snd j))) journeys' 
      scale = fromIntegral (maximum scales) / 450
      scale' = fromIntegral (maximum scales) / 450
      scl = max scale scale'
      img = renderDrawing 1000 1000 black $ do
              withTexture (uniformTexture blue) $
                mapM_ (renderJourney (500,500) scale) journeys
              withTexture (uniformTexture green) $
                mapM_ (renderJourney (500,500) scale) journeys'
  
  writePng "/home/thomas/Projects/personal/drivers/resources/journeys.png" img
