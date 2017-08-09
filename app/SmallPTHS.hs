{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Maybe (mapMaybe, listToMaybe)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Foldable (for_)
import System.IO (hFlush, stdout, withFile, IOMode(..), hPutStr)
import System.Random.MWC (create, uniform, Gen)
import Text.Read (readMaybe)
import Data.Foldable (foldlM)
import System.Environment (getArgs)
import GHC.Prim (RealWorld)
import qualified Data.Vector.Mutable as MV
import Data.Coerce

------------------------------------------------------
-- Vec private API
------------------------------------------------------

data Vec = Vec {
  getX :: !Double,
  getY :: !Double,
  getZ :: !Double
  } deriving (Show, Eq)

blackVec :: Vec
blackVec = Vec 0 0 0

-- term by term addition, substraction, product
(.+.) :: Vec -> Vec -> Vec
(.-.) :: Vec -> Vec -> Vec
(.*.) :: Vec -> Vec -> Vec
(.%.) :: Vec -> Vec -> Vec

infixl 6 .+.
infixl 6 .-.
infixl 7 .*.
infixl 7 .*

(Vec a b c) .+. (Vec a' b' c') = Vec (a + a') (b + b') (c + c')
(Vec a b c) .-. (Vec a' b' c') = Vec (a - a') (b - b') (c - c')
(Vec a b c) .*. (Vec a' b' c') = Vec (a * a') (b * b') (c * c')
(Vec x y z) .%. (Vec x' y' z') = Vec (y*z'-z*y') (z*x'-x*z') (x*y'-y*x')

-- product with a scalar
(.*) :: Vec -> Double -> Vec

(Vec a b c) .* s = Vec (a * s) (b * s) (c * s)

normalize :: Direction -> NormalizedDirection
normalize (Direction(v@(Vec a b c))) = NormalizedDirection (v .* (1 / norm))
  where norm = sqrt (a * a + b * b + c * c)

dot :: Vec -> Vec -> Double
dot (Vec a b c) (Vec a' b' c') = a * a' + b * b' + c * c'

----------------------------------------------------------

newtype Position = Position Vec deriving (Show, Eq)
newtype Direction = Direction Vec deriving (Show, Eq)
newtype NormalizedDirection = NormalizedDirection Vec deriving (Show)

directionFromTo :: Position -> Position -> Direction
directionFromTo (Position from) (Position to) = Direction (to .-. from)
project :: Direction -> NormalizedDirection -> Double
project (Direction d) (NormalizedDirection nd) = d `dot` nd

cosinus :: NormalizedDirection -> NormalizedDirection -> Double
cosinus (NormalizedDirection d) (NormalizedDirection nd) = d `dot` nd

norm2 :: Direction -> Double
norm2 (Direction d) = d `dot` d

data Ray = Ray {origin :: !Position, direction :: !NormalizedDirection} deriving (Show)

data Refl_t = DIFF | SPEC | REFR deriving (Show, Eq)

data Sphere = Sphere {
  radius :: !Double,
  position :: !Position,
  emission :: !Vec,
  color :: !Vec,
  refl :: !Refl_t
  } deriving (Show, Eq)

data Intersect = Intersect {
  getObj :: !Sphere,
  getT :: !Double
  } deriving (Show)

intersectSphere :: Ray -> Sphere -> Maybe Intersect
intersectSphere Ray{..} s@Sphere{..} =
  let
    op = directionFromTo origin position
    eps = 0.0001
    b = project op direction
    det' = b * b - norm2 op + radius * radius

    det = sqrt det'
    ta = b - det
    tb = b + det
  in
    if
      | det' < 0 -> Nothing
      | ta > eps -> Just (Intersect s ta)
      | tb > eps -> Just (Intersect s tb)
      | otherwise -> Nothing

mkPosition :: Double -> Double -> Double -> Position
mkPosition x y z = Position (Vec x y z)

spheres :: [Sphere]
spheres = [
   Sphere 1e5  (mkPosition  (1e5+1) 40.8 81.6)  blackVec (Vec 0.75 0.25 0.25) DIFF -- Left
  ,Sphere 1e5  (mkPosition (-1e5+99) 40.8 81.6) blackVec (Vec 0.25 0.25 0.75) DIFF -- Rght
  ,Sphere 1e5  (mkPosition 50 40.8  1e5)      blackVec (Vec 0.75 0.75 0.75) DIFF -- Back
  ,Sphere 1e5  (mkPosition 50 40.8 (-1e5+170))  blackVec blackVec            DIFF -- Frnt
  ,Sphere 1e5  (mkPosition 50  1e5  81.6)     blackVec (Vec 0.75 0.75 0.75) DIFF -- Botm
  ,Sphere 1e5  (mkPosition 50 (-1e5+81.6) 81.6) blackVec (Vec 0.75 0.75 0.75) DIFF -- Top
  ,Sphere 16.5 (mkPosition 27 16.5 47)        blackVec ((Vec 1 1 1).*0.999)  SPEC -- Mirr
  ,Sphere 16.5 (mkPosition 73 16.5 78)        blackVec ((Vec 1 1 1).*0.999)  REFR -- Glas
  ,Sphere 1.5  (mkPosition 50 (81.6-16.5) 81.6) ((Vec 4 4 4).*100)   blackVec  DIFF -- Lite
  ]

clamp :: (Num a, Ord a) => a -> a
clamp x = min 1 (max 0 x)

toInt :: Double -> Int
toInt x = truncate (((clamp x) ** (1 / 2.2)) * 255 + 0.5)

intersectScene :: Ray -> Maybe Intersect
intersectScene ray =
  let
    its = mapMaybe (intersectSphere ray) spheres
  in
    case its of
      [] -> Nothing
      _ -> Just (minimumBy (comparing getT) its)

radianceDiffuse :: Gen RealWorld -> Ray -> Int -> IO Vec
radianceDiffuse gen r depth = radiance gen r depth 1
radianceSpecular :: Gen RealWorld -> Ray -> Int -> IO Vec
radianceSpecular gen r depth = radiance gen r depth 0

getDiffuseDirect :: Gen RealWorld -> Position -> NormalizedDirection -> IO Vec
getDiffuseDirect gen x nl = foldlM fFold blackVec spheres
  where
    fFold accum s@Sphere{..}
      | getX emission <= 0 && getY emission <= 0 && getZ emission <= 0 = pure accum
      | otherwise = do
          eps1 <- uniform gen
          eps2 <- uniform gen
          let sw = directionFromTo x position
              su = normalize $ coerce $ ((if abs (getX $ coerce sw) > 0.1 then Vec 0 1 0 else Vec 1 0 0) .%. coerce sw)
              sv = coerce sw .%. coerce su
              cos_a_max = sqrt (1 - radius ^ (2 :: Int) / (norm2 (directionFromTo position x)))

              cos_a = 1 - eps1 + eps1 * cos_a_max
              sin_a = sqrt (1 - cos_a * cos_a)
              phi = 2 * pi * eps2
              l = normalize $ coerce $ ((coerce su .* (cos phi * sin_a)) .+. (sv .* (sin phi * sin_a)) .+. (coerce sw .* cos_a))
          case intersectScene (Ray x l) of
            Nothing -> pure accum
            Just it ->  if getObj it == s
                        then let omega = 2 * pi * (1 - cos_a_max)
                             in pure (accum .+. (emission .* (l `cosinus` nl * omega / pi)))
                        else pure accum

getDiffuseIndirect :: Gen RealWorld -> NormalizedDirection -> Position -> IO (Ray, Double)
getDiffuseIndirect gen nl x = do
                  let pi2 = (2 * pi) :: Double
                  r1 <- (pi2*) <$> uniform gen
                  r2 <- uniform gen
                  let r2s = sqrt r2
                      w = nl
                      u = normalize (coerce ((if abs (getX (coerce w)) > 0.1 then Vec 0 1 0 else Vec 1 0 0) .%. coerce w))
                      v = coerce w .%. coerce u
                      d = normalize (coerce ((coerce u .* (cos r1 * r2s)) .+.
                                     (coerce v .* (sin r1 * r2s)) .+.
                                     (coerce w .* (sqrt (1 - r2)))))

                  pure ((Ray x d), 1)

reflect :: NormalizedDirection -> NormalizedDirection -> NormalizedDirection
reflect (NormalizedDirection direction) (NormalizedDirection n) = NormalizedDirection (direction .-. (n .* (2 * n `dot` (direction))))

getSpecularIndirect :: Position -> NormalizedDirection -> NormalizedDirection -> IO (Ray, Double)
getSpecularIndirect x direction n = pure (Ray x (reflect direction n), 1)

getRefractionIndirect :: Gen RealWorld -> NormalizedDirection -> Position -> NormalizedDirection -> NormalizedDirection -> IO (Ray, Double)
getRefractionIndirect gen nl x direction n = do
                  let reflRay = Ray x (coerce (coerce direction .-. (coerce n .* (2 * (coerce n `dot` (coerce direction))))))
                      into = n `cosinus` nl > 0
                      nc = 1
                      nt = 1.5
                      nnt = if into then (nc / nt) else (nt / nc)
                      ddn = direction `cosinus` nl
                      cos2t = 1 - nnt * nnt * (1 - ddn * ddn)

                  if cos2t < 0
                    then do
                       pure (reflRay, 1)
                    else do
                      let tdir = normalize (coerce ((coerce direction .* nnt) .-. (coerce n .* ((if into then 1 else (-1)) * (ddn * nnt + sqrt cos2t)))))
                          a = nt - nc
                          b = nt + nc
                          r0 = (a / b) ^ (2 :: Int)
                          c = 1 - if into then -ddn else (tdir `cosinus` n)
                          re = r0 + (1-r0) * (c ^ (5 :: Int))
                          tr = 1 - re
                          p'' = 0.25 + 0.5 * re
                          rp = re / p''
                          tp = tr / (1 - p'')
                      rnd <- uniform gen

                      pure $ if rnd < p''
                        then (reflRay, rp)
                        else (Ray x tdir, tp)

reflectBSDF :: Gen RealWorld -> Sphere -> NormalizedDirection -> Position -> Int -> NormalizedDirection -> NormalizedDirection -> Vec -> IO Vec
reflectBSDF gen obj nl x depth direction n f = do
              case refl obj of
                DIFF -> do
                  direct <- getDiffuseDirect gen x nl
                  (iRay, iWeight) <- getDiffuseIndirect gen nl x

                  indirect <- radiance gen iRay (depth + 1) 1
                  pure (f .*. (direct .+. indirect .* iWeight))
                SPEC -> do
                  (iRay, iWeight) <- getSpecularIndirect x direction n

                  indirect <- radiance gen iRay (depth + 1) 1

                  pure (f .*. indirect .* iWeight)
                REFR -> do
                  (iRay, iWeight) <- getRefractionIndirect gen nl x direction n

                  indirect <- radiance gen iRay (depth + 1) 1

                  pure (f .*. indirect .* iWeight)

radiance :: Gen RealWorld -> Ray -> Int -> Int -> IO Vec
radiance gen r@Ray{..} depth e' = do
  case intersectScene r of
    Nothing -> pure blackVec
    Just (Intersect obj t) -> do
      let x = origin `translate` (coerce (coerce direction .* t))
          n = normalize (directionFromTo (position obj) x)
          nl = if direction `cosinus` n < 0 then n else coerce (coerce n .* (-1))
          f = color obj
          p' = if getX f > getY f && getX f > getZ f
                  then getX f
                  else if getY f > getZ f
                     then getY f
                     else getZ f

          ref = reflectBSDF gen obj nl x depth direction n
      if (depth + 1) > 5 || p' == 0
        then do
          rv <- uniform gen
          if rv < p'
            then do
              res <- ref (f .* (1 / p'))
              pure (emission obj .* (fromIntegral e') .+. res)
            else pure (emission obj .* (fromIntegral e'))
        else do
           res <- ref f
           pure (emission obj .* (fromIntegral e') .+. res)

mkDirection :: Double -> Double -> Double -> Direction
mkDirection a b c = Direction (Vec a b c)

main :: IO ()
main = do
  args <- getArgs

  let w = 1024 :: Int
      h = 768 :: Int
      samps = maybe 1 (`div`4) (listToMaybe args >>= readMaybe) :: Int
      cam = Ray (mkPosition 50 52 295.6) (normalize (mkDirection 0 (-0.042612) (-1)))
      cx = Vec (fromIntegral w * 0.5135 / fromIntegral h) 0 0
      cy = (coerce (normalize (coerce (cx .%. coerce (direction cam))))) .* 0.5135

  gen <- create

  c <- MV.replicate (h * w) blackVec
  for_ [0 .. (h - 1)] $ \y -> do
    putStr ("\rRendering (" ++ show (samps * 4) ++ " spp) " ++ show ((100 * fromIntegral y / (fromIntegral h - 1)) :: Double) ++ "%.")
    hFlush stdout
    for_ [0..(w - 1)] $ \x -> do
       let i = (h - y - 1) * w + x
       for_ [0..1] $ \sy ->
          for_ [0..1] $ \sx -> do
             let fFold accum _ = do
                         r1 <- (2*) <$> uniform gen
                         r2 <- (2*) <$> uniform gen
                         let dx = if r1 < 1 then sqrt(r1) -1 else 1 - sqrt(2 - r1)
                             dy = if r2 < 1 then sqrt(r2) -1 else 1 - sqrt(2 - r2)

                             d' = (cx .* (((sx + 0.5 + dx) / 2 + fromIntegral x) / fromIntegral w - 0.5)) .+.
                                  (cy .* (((sy + 0.5 + dy) / 2 + fromIntegral y) / fromIntegral h - 0.5)) .+. (coerce direction cam)
                         rad <- radiance gen (Ray (origin cam `translate` (coerce (d' .* 140))) (normalize (coerce d'))) 0 1
                         pure $ accum .+. rad
             rad <- foldlM fFold blackVec [0..(samps - 1)]
             let clampedRes = clampV rad .* 0.25

             old <- MV.unsafeRead c i
             MV.unsafeWrite c i $! (old .+. clampedRes)

  let header = ("P3\n" ++ show w ++ " " ++ show h ++ "\n255\n")

  withFile  "image_hs.ppm" WriteMode $ \handle -> do
    hPutStr handle header
    for_ [0..(w * h) - 1] $ \i -> do
      v <- MV.unsafeRead c i
      hPutStr handle (v2c v)

translate :: Position -> Direction -> Position
translate (Position p) (Direction d) = Position (p .+. d)

v2c :: Vec -> [Char]
v2c (Vec a b c) = show (toInt a) ++ " " ++ show (toInt b) ++ " " ++ show (toInt c) ++ " "

clampV :: Vec -> Vec
clampV (Vec a b c) = Vec (clamp a) (clamp b) (clamp c)
