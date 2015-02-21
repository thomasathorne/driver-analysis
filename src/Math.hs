
module Math where

import qualified Data.Vector.Unboxed as V



-----------------

journeyTime :: V.Vector (Double,Double) -> Double
journeyTime = fromIntegral . V.length

journeyDist :: V.Vector (Double,Double) -> Double
journeyDist = mag . V.last

differentiate :: V.Vector (Double,Double) -> V.Vector (Double,Double)
differentiate v =
  let v' = V.drop 1 v
  in V.zipWith disp v v'

---------------

mag :: (Double, Double) -> Double
mag (x,y) = sqrt $ x^2 + y^2

disp :: (Double, Double) -> (Double, Double) -> (Double, Double)
disp (x,y) (z,w) = (z-x, w-y)

rotate :: (Double,Double) -> (Double, Double) -> (Double, Double)
rotate (rx,ry) (x,y) = (rx*x - ry*y, ry*x + rx*y)

translate :: (Double,Double) -> (Double, Double) -> (Double, Double)
translate (tx,ty) (x,y) = (tx+x,ty+y)

inverse :: (Double,Double) -> (Double, Double)
inverse p@(x,y) = let d2 = x^2 + y^2
                  in (x/d2, -y/d2)

upperHalf :: (Double,Double) -> (Double, Double)
upperHalf (x,y) = (x,abs y) 
                     
