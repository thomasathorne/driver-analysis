{-# LANGUAGE OverloadedStrings #-}

module Reader where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Vector.Unboxed as V

dataPath = "/home/thomas/Projects/personal/drivers/resources/drivers/"
journeyPath d n = dataPath ++ d ++ "/" ++ show n ++ ".csv"

getJourney :: String -> Int -> IO (V.Vector (Double,Double))
getJourney d n = readJourney $ journeyPath d n

readJourney :: FilePath -> IO (V.Vector (Double, Double))
readJourney path = do
  s' <- L.readFile path
  let (_ ,s) = readLine s'
  return $ parse s

parse = V.unfoldr step
  where step s = case readLine s of
          (Just (a,b), rest) -> Just ((a,b),rest)
          (Nothing, _) -> Nothing

readLine :: L.ByteString -> (Maybe (Double, Double), L.ByteString)
readLine s =
  let (line, rest) = L.break (=='\n') s
      mab = do (aTens, dotA) <- L.readInt line
               (aOnes, comma) <- L.readInt $ L.drop 1 dotA
               (bTens, dotB) <- L.readInt $ L.drop 1 comma
               (bOnes, _) <- L.readInt $ L.drop 1 dotB
               return (fromIntegral aTens + 0.1*fromIntegral aOnes,
                       fromIntegral bTens + 0.1*fromIntegral bOnes)
  in (mab, L.drop 1 rest)
