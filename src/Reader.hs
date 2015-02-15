{-# LANGUAGE OverloadedStrings #-}

module Reader where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Vector.Unboxed as V


readJourney :: FilePath -> IO (V.Vector (Int, Int))
readJourney path = do
  s' <- L.readFile path
  let (_ ,s) = readLine s'
  return $! parse s

parse = V.unfoldr step
  where step s = case readLine s of
          (Just (a,b), rest) -> Just ((a,b),rest)
          (Nothing, _) -> Nothing

readLine :: L.ByteString -> (Maybe (Int, Int), L.ByteString)
readLine s =
  let (line, rest) = L.break (=='\n') s
      mab = do (aTens, dotA) <- L.readInt line
               (aOnes, comma) <- L.readInt $ L.drop 1 dotA
               (bTens, dotB) <- L.readInt $ L.drop 1 comma
               (bOnes, _) <- L.readInt $ L.drop 1 dotB
               return (aTens*10 + aOnes, bTens*10 + bOnes)
  in (mab, L.drop 1 rest)
