{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Main where

import Reader
import Math
import Control.Lens

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Vector.Unboxed as V
import System.Directory (getDirectoryContents)
import Data.String (IsString(fromString))


main = do ds <- getDirectoryContents dataPath <&> drop 2 <&> take 54 -- 2% of total
          flip traverse ds $ \d -> driver d 1
  where
    driver d n | n > 200 = return ()
               | otherwise = do !j <- getJourney d n
                                L.putStrLn $ L.pack $ d ++ "_" ++ show n ++ "," ++
                                  show (journeyDist j)
                                driver d $ n+1
      
