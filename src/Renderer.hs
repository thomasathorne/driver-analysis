{-# LANGUAGE BangPatterns #-}

module Main where

import Reader
import Math

import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Codec.Picture( PixelRGBA8(..), writePng, Image )
import qualified Data.Vector.Unboxed as V
import Control.Applicative
import System.Directory
import Control.Lens ((<&>),(&))
import Data.Array.IO
import Data.Array.MArray
import Control.Concurrent
import Control.Concurrent.ParallelIO
import Control.Concurrent.STM

speedRange :: Double -> Bool
speedRange s = s > 35 && s < 40

black = PixelRGBA8 0 0 0 255
blue = PixelRGBA8 0 0 255 80
green = PixelRGBA8 0 255 0 80
red = PixelRGBA8 255 0 0 80
white = PixelRGBA8 255 255 255 80
logColour base r g b = PixelRGBA8 (floor (100*(logBase base (fromIntegral r+1))))
                       (floor (100*(logBase base (fromIntegral g+1))))
                       (floor (100*(logBase base (fromIntegral b+1))))
                       255

float :: Double -> Float
float = fromRational.toRational

asPoint :: (Double,Double) -> Point
asPoint (a,b) = V2 (float a) (float b)

renderJourney :: (Double,Double) -> Double -> V.Vector (Double,Double) -> Drawing px ()
renderJourney (x,y) scale v =
  let scaled = V.map (\(a,b) -> (a/scale, b/scale)) v
      (a,b) = V.last scaled
      m = mag (a,b)
      rotation = (a/m, -b/m)
      rotated = V.map (rotate rotation) scaled
      translated = V.map (\(a,b) -> (x+a, y+b)) rotated
      v2s = map asPoint $ V.toList translated
      v2s' = tail v2s
      primitives = LinePrim <$> zipWith Line v2s v2s'
  in stroke 0.5 JoinRound (CapRound, CapRound) primitives

threeDriversImage :: String -> String -> String -> IO (Image PixelRGBA8)
threeDriversImage a b c = do
  journeys <- mapM (getJourney a) [1..200]
  journeys' <- mapM (getJourney b) [1..200]
  journeys'' <- mapM (getJourney c) [1..200] 
  let scales = map (\j -> max (V.maximum (V.map fst j)) (V.maximum (V.map snd j))) journeys
      scales' = map (\j -> max (V.maximum (V.map fst j)) (V.maximum (V.map snd j))) journeys'
      scales'' = map (\j -> max (V.maximum (V.map fst j)) (V.maximum (V.map snd j))) journeys''
      scale = maximum scales / 400
      scale' = maximum scales / 400
      scale'' = maximum scales / 400  
      scl = scale `max` scale' `max` scale''
  return $ renderDrawing 1000 1000 black $ do
    withTexture (uniformTexture red) $
      mapM_ (renderJourney (100,200) scale) journeys
    withTexture (uniformTexture green) $
      mapM_ (renderJourney (100,500) scale) journeys'
    withTexture (uniformTexture blue) $
      mapM_ (renderJourney (100,800) scale) journeys''

drawLine (a,b) (c,d) = do
  stroke 1 JoinRound (CapRound, CapRound)
      [LinePrim $ Line (V2 a b) (V2 c d)]

plotDriver :: IOUArray (Int,Int) Int -> IOUArray (Int,Int) Int
              -> IOUArray (Int,Int) Int -> String -> IO ()
plotDriver p1 p2 p3 s =
  flip mapM_ [1..200] $ \n -> n & getJourney s >>= fountainPlot p1 p2 p3

plotHelperLines = withTexture (uniformTexture white) $ do
  drawLine (200,800) (0,800)
  drawLine (400, 800) (400, 900)
  drawLine (600, 800) (600, 900)
  drawLine (800, 800) (800, 900)

fountainPlot :: IOUArray (Int,Int) Int -> IOUArray (Int,Int) Int
                -> IOUArray (Int,Int) Int -> V.Vector (Double,Double) -> IO ()
fountainPlot rplot gplot bplot journey = do
  let speeds = V.map mag $ differentiate journey
      plus1 = V.drop 1 journey
      plus2 = V.drop 2 journey
      plus3 = V.drop 3 journey
      plus4 = V.drop 4 journey
      allTogether = V.zip6 journey speeds plus1 plus2 plus3 plus4
      filtered = flip V.filter allTogether $ \(_,s,_,_,_,_) -> speedRange s
      dot (p0, s, p1, p2, p3, p4) =
        let vel = disp p0 p1
            step1 = disp p1 p2
            step2 = disp p1 p3
            step3 = disp p1 p4
        in (rotate (inverse vel) step1 & upperHalf & rotate (20,0),
            rotate (inverse vel) step2 & upperHalf & rotate (20,0),
            rotate (inverse vel) step3 & upperHalf & rotate (20,0))
      (rs,gs,bs) = V.unzip3 $ V.map dot filtered
  cloudPlot rplot rs
  cloudPlot gplot gs
  cloudPlot bplot bs

cloudPlot :: IOUArray (Int,Int) Int -> V.Vector (Double,Double) -> IO ()
cloudPlot plot dots = V.forM_ dots $ addToPlot plot

addToPlot :: IOUArray (Int,Int) Int -> (Double,Double) -> IO ()
addToPlot plot (x,y) = do
  let (a,b) = (floor x, floor y)
  if a <= 80 && a >= 0 && b <= 80 && b >= 0
    then do n <- readArray plot (a,b)
            writeArray plot (a,b) $ n+1
    else return ()

runPlots :: IOUArray (Int,Int) Int -> IOUArray (Int,Int) Int
            -> IOUArray (Int,Int) Int -> IO (Image PixelRGBA8)
runPlots r g b = do
  rs <- getAssocs r
  gs <- getAssocs g
  bs <- getAssocs b
  let irgbs = zipWith3 (\(i,rn) (_,gn) (_,bn) -> (i,rn,gn,bn)) rs gs bs
      
  writeFile "/home/thomas/Projects/personal/drivers/resources/prob_dist.dat" $ show irgbs
  
  return $ renderDrawing 1000 1000 black $ do
    flip mapM_ irgbs $ \((x,y),r',g',b') -> do
      let p = V2 (fromIntegral (200 + 10 * x)) (fromIntegral (790 - 10 * y))
      withTexture (uniformTexture $ logColour 1000 r' g' b') $
        fill $ rectangle p 10 10
    plotHelperLines

accDrivers :: TChan String -> [String] ->
              IO (IOUArray (Int,Int) Int,IOUArray (Int,Int) Int,IOUArray (Int,Int) Int)
accDrivers msgChan ds = do
  rplot <- newArray ((0,0),(80,80)) 0 :: IO (IOUArray (Int,Int) Int)
  gplot <- newArray ((0,0),(80,80)) 0 :: IO (IOUArray (Int,Int) Int)
  bplot <- newArray ((0,0),(80,80)) 0 :: IO (IOUArray (Int,Int) Int)
  
  flip mapM_ ds $ \d -> do
    plotDriver rplot gplot bplot d
    atomically $ writeTChan msgChan $ "driver " ++ d

  return (rplot, gplot, bplot)
  

listen :: TChan String -> Int -> IO ()
listen chan n = do
  s <- atomically $ readTChan chan
  putStrLn $ s ++ "  (number " ++ show n ++ ")"
  listen chan $ n+1

addArrays :: [IOUArray (Int,Int) Int] -> IO (IOUArray (Int,Int) Int)
addArrays ars = do
  sums <- newArray ((0,0),(80,80)) 0 :: IO (IOUArray (Int,Int) Int)
  flip mapM_ [1..80] $ \x ->
    flip mapM_ [1..80] $ \y -> do
      ns <- flip mapM ars $ \a -> readArray a (x,y)
      writeArray sums (x,y) (sum ns)
  return sums

cutUpList :: [a] -> Int -> [[a]]
cutUpList as 1 = [as]
cutUpList as n =
  let m = length as `div` n
  in take m as : cutUpList (drop m as) (n-1)

main = do
  ds <- getDirectoryContents dataPath <&> drop 2
  let dds = cutUpList ds 4
  
  chan <- atomically newTChan

  forkIO $ listen chan 1
  (rs,gs,bs) <- unzip3 <$> parallel (map (accDrivers chan) dds)

  r <- addArrays rs
  g <- addArrays gs
  b <- addArrays bs
  
  img <- runPlots r g b
  writePng "/home/thomas/Projects/personal/drivers/resources/fountain35-40.png" img
