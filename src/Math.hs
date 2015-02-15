
module Math where

import qualified Data.Vector.Unboxed as V

journeyTime :: V.Vector (Int,Int) -> Int
journeyTime = V.length

journeyDist :: V.Vector (Int,Int) -> Double
journeyDist = mag . V.last

velocities :: V.Vector (Int,Int) -> V.Vector (Int,Int)
velocities positions = undefined

---------------

mag :: (Int, Int) -> Double
mag (x,y) = sqrt $ (fromIntegral x)^2 + (fromIntegral y)^2

disp :: (Int,Int) -> (Int,Int) -> (Int,Int)
disp (x,y) (z,w) = (z-x, w-y)
