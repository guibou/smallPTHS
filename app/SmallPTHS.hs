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

normalize :: Vec -> Vec
normalize v@(Vec a b c) = v .* (1 / norm)
  where norm = sqrt (a * a + b * b + c * c)

dot :: Vec -> Vec -> Double
dot (Vec a b c) (Vec a' b' c') = a * a' + b * b' + c * c'

data Ray = Ray {origin :: !Vec, direction :: !Vec} deriving (Show)

data Refl_t = DIFF | SPEC | REFR deriving (Show, Eq)

data Sphere = Sphere {
  radius :: !Double,
  position :: !Vec,
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
    op = position .-. origin
    eps = 0.0001
    b = op `dot` direction
    det' = b * b - op `dot` op + radius * radius

    det = sqrt det'
    ta = b - det
    tb = b + det
  in
    if
      | det' < 0 -> Nothing
      | ta > eps -> Just (Intersect s ta)
      | tb > eps -> Just (Intersect s tb)
      | otherwise -> Nothing

spheres :: [Sphere]
spheres = [
   Sphere 1e5  (Vec  (1e5+1) 40.8 81.6)  blackVec (Vec 0.75 0.25 0.25) DIFF -- Left
  ,Sphere 1e5  (Vec (-1e5+99) 40.8 81.6) blackVec (Vec 0.25 0.25 0.75) DIFF -- Rght
  ,Sphere 1e5  (Vec 50 40.8  1e5)      blackVec (Vec 0.75 0.75 0.75) DIFF -- Back
  ,Sphere 1e5  (Vec 50 40.8 (-1e5+170))  blackVec blackVec            DIFF -- Frnt
  ,Sphere 1e5  (Vec 50  1e5  81.6)     blackVec (Vec 0.75 0.75 0.75) DIFF -- Botm
  ,Sphere 1e5  (Vec 50 (-1e5+81.6) 81.6) blackVec (Vec 0.75 0.75 0.75) DIFF -- Top
  ,Sphere 16.5 (Vec 27 16.5 47)        blackVec ((Vec 1 1 1).*0.999)  SPEC -- Mirr
  ,Sphere 16.5 (Vec 73 16.5 78)        blackVec ((Vec 1 1 1).*0.999)  REFR -- Glas
  ,Sphere 1.5  (Vec 50 (81.6-16.5) 81.6) ((Vec 4 4 4).*100)   blackVec  DIFF -- Lite
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

getDiffuseDirect gen x nl = foldlM fFold blackVec spheres
  where
    fFold accum s@Sphere{..}
      | getX emission <= 0 && getY emission <= 0 && getZ emission <= 0 = pure accum
      | otherwise = do
          eps1 <- uniform gen
          eps2 <- uniform gen
          let sw = position .-. x
              su = normalize ((if abs (getX sw) > 0.1 then Vec 0 1 0 else Vec 1 0 0) .%. sw)
              sv = sw .%. su
              cos_a_max = sqrt (1 - radius ^ 2 / ((x .-. position) `dot` (x .-. position)))

              cos_a = 1 - eps1 + eps1 * cos_a_max
              sin_a = sqrt (1 - cos_a * cos_a)
              phi = 2 * pi * eps2
              l = normalize ((su .* (cos phi * sin_a)) .+. (sv .* (sin phi * sin_a)) .+. (sw .* cos_a))
          case intersectScene (Ray x l) of
            Nothing -> pure accum
            Just it ->  if getObj it == s
                        then let omega = 2 * pi * (1 - cos_a_max)
                             in pure (accum .+. (emission .* (l `dot` nl * omega / pi)))
                        else pure accum

radiance :: Gen RealWorld -> Ray -> Int -> Int -> IO Vec
radiance gen r@Ray{..} depth e' = do
  case intersectScene r of
    Nothing -> pure blackVec
    Just (Intersect obj t) -> do
      let x = origin .+. (direction .* t)
          n = normalize (x .-. position obj)
          nl = if n `dot` (direction) < 0 then n else (n .* (-1))
          f = color obj
          p' = if getX f > getY f && getX f > getZ f
                  then getX f
                  else if getY f > getZ f
                     then getY f
                     else getZ f
          reflect f = do
              case refl obj of
                DIFF -> do
                  let pi2 = (2 * pi) :: Double
                  r1 <- (pi2*) <$> uniform gen
                  r2 <- uniform gen
                  let r2s = sqrt r2
                      w = nl
                      u = normalize ((if abs (getX w) > 0.1 then Vec 0 1 0 else Vec 1 0 0) .%. w)
                      v = w .%. u
                      d = normalize ((u .* (cos r1 * r2s)) .+.
                                     (v .* (sin r1 * r2s)) .+.
                                     (w .* (sqrt (1 - r2))))

                  direct <- (f .*.) <$> getDiffuseDirect gen x nl
                  indirect <- radianceDiffuse gen (Ray x d) (depth + 1)
                  pure ((emission obj .* (fromIntegral e')) .+. direct .+. (f .*. indirect))
                SPEC -> do
                  rad' <- radianceSpecular gen (Ray x (direction .-. (n .* (2 * n `dot` (direction))))) (depth + 1)
                  pure (emission obj .+. (f .*. rad'))
                REFR -> do
                  let reflRay = Ray x (direction .-. (n .* (2 * (n `dot` (direction)))))
                      into = n `dot` nl > 0
                      nc = 1
                      nt = 1.5
                      nnt = if into then (nc / nt) else (nt / nc)
                      ddn = direction `dot` nl
                      cos2t = 1 - nnt * nnt * (1 - ddn * ddn)

                  if cos2t < 0
                    then do
                       rad' <- radianceDiffuse gen reflRay (depth + 1)
                       pure (emission obj .+. (f .*. rad'))
                    else do
                      let tdir = normalize ((direction .* nnt) .-. (n .* ((if into then 1 else (-1)) * (ddn * nnt + sqrt cos2t))))
                          a = nt - nc
                          b = nt + nc
                          r0 = (a / b) ^ 2
                          c = 1 - if into then -ddn else (tdir `dot` n)
                          re = r0 + (1-r0) * (c ^ 5)
                          tr = 1 - re
                          p'' = 0.25 + 0.5 * re
                          rp = re / p''
                          tp = tr / (1 - p'')

                          reflRadiance = radianceDiffuse gen reflRay (depth + 1)
                          refrRadiance = radianceDiffuse gen (Ray x tdir) (depth + 1)

                      rnd <- uniform gen

                      subrad <- if (depth + 1) > 2
                                   then if rnd < p''
                                        then do
                                          rad <- reflRadiance
                                          pure (rad .* rp)
                                        else do
                                           rad <- refrRadiance
                                           pure (rad .* tp)
                                   else do
                                       radA <- reflRadiance
                                       radB <- refrRadiance

                                       pure (radA .* re .+. radB .* tr)

                      pure (emission obj .+. (f .*. subrad))
      if (depth + 1) > 5 || p' == 0
        then do
          rv <- uniform gen
          if rv < p'
            then reflect (f .* (1 / p'))
            else pure (emission obj .* (fromIntegral e'))
        else reflect f

main :: IO ()
main = do
  args <- getArgs

  let w = 1024 :: Int
      h = 768 :: Int
      samps = maybe 1 (`div`4) (listToMaybe args >>= readMaybe) :: Int
      cam = Ray (Vec 50 52 295.6) (normalize (Vec 0 (-0.042612) (-1)))
      cx = Vec (fromIntegral w * 0.5135 / fromIntegral h) 0 0
      cy = (normalize (cx .%. direction cam)) .* 0.5135

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
                                  (cy .* (((sy + 0.5 + dy) / 2 + fromIntegral y) / fromIntegral h - 0.5)) .+. direction cam
                         rad <- radiance gen (Ray (origin cam .+. (d' .* 140)) (normalize d')) 0 1
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

v2c :: Vec -> [Char]
v2c (Vec a b c) = show (toInt a) ++ " " ++ show (toInt b) ++ " " ++ show (toInt c) ++ " "

clampV :: Vec -> Vec
clampV (Vec a b c) = Vec (clamp a) (clamp b) (clamp c)
