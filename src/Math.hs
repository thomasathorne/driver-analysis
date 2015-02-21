
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

