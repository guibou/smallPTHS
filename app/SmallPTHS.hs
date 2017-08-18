{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Data.Maybe (mapMaybe, listToMaybe, isJust)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Foldable (for_)
import System.IO (hFlush, stdout, withFile, IOMode(..), hPutStr)
import System.Random.MWC (create, uniform, Gen)
import Text.Read (readMaybe)
import Data.Foldable (foldlM, foldl')
import System.Environment (getArgs)
import GHC.Prim (RealWorld)
import qualified Data.Vector.Mutable as MV
import Control.Concurrent.Async
import Control.Monad (when)

------------------------------------------------------
-- Vec private API
------------------------------------------------------

data Vec = Vec {
  getX :: !Double,
  getY :: !Double,
  getZ :: !Double
  } deriving (Show)

pattern Black :: Color
pattern Black = Color (Vec 0 0 0)

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

normalize :: Direction 'NotNormalized -> Direction 'NotNormalized
normalize (Direction(v@(Vec a b c))) = Direction (v .* (1 / norm))
  where norm = sqrt (a * a + b * b + c * c)

dot :: Vec -> Vec -> Double
dot (Vec a b c) (Vec a' b' c') = a * a' + b * b' + c * c'

----------------------------------------------------------

newtype Position = Position Vec deriving (Show)

data NormalizedStatus = NotNormalized | Normalized

newtype Direction (k :: NormalizedStatus) = Direction Vec deriving (Show)

newtype Color = Color Vec deriving (Show)

directionFromTo :: Position -> Position -> Direction 'NotNormalized
directionFromTo (Position from) (Position to) = Direction (to .-. from)
project :: Direction k -> Direction 'NotNormalized -> Double
project (Direction d) (Direction nd) = d `dot` nd

cosinus :: Direction 'NotNormalized -> Direction 'NotNormalized -> Double
cosinus (Direction d) (Direction nd) = d `dot` nd

norm2 :: Direction 'NotNormalized -> Double
norm2 (Direction d) = d `dot` d

isBlack :: Color -> Bool
isBlack Black = True
isBlack _ = False

--- Sampling
data Sample2D = Sample2D !Double !Double
  deriving (Show)

sample2D :: Gen RealWorld -> IO Sample2D
sample2D gen = Sample2D <$> uniform gen <*> uniform gen
---

data Ray = Ray {origin :: !Position, direction :: !(Direction 'NotNormalized)} deriving (Show)

data Refl_t = DIFF | SPEC | REFR deriving (Show)

data Sphere = Sphere {
  radius :: !Double,
  position :: !Position,
  emission :: !Color,
  color :: !Color,
  refl :: !Refl_t
  } deriving (Show)

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

mkColor :: Double -> Double -> Double -> Color
mkColor x y z = Color (Vec x y z)

scaleColor :: Color -> Double -> Color
scaleColor (Color c) f = Color (c .* f)

spheres :: [Sphere]
spheres = [
   Sphere 1e5  (mkPosition  (1e5+1) 40.8 81.6)  Black (mkColor 0.75 0.25 0.25) DIFF -- Left
  ,Sphere 1e5  (mkPosition (-1e5+99) 40.8 81.6) Black (mkColor 0.25 0.25 0.75) DIFF -- Rght
  ,Sphere 1e5  (mkPosition 50 40.8  1e5)      Black (mkColor 0.75 0.75 0.75) DIFF -- Back
  ,Sphere 1e5  (mkPosition 50 40.8 (-1e5+170))  Black Black            DIFF -- Frnt
  ,Sphere 1e5  (mkPosition 50  1e5  81.6)     Black (mkColor 0.75 0.75 0.75) DIFF -- Botm
  ,Sphere 1e5  (mkPosition 50 (-1e5+81.6) 81.6) Black (mkColor 0.75 0.75 0.75) DIFF -- Top
  ,Sphere 16.5 (mkPosition 27 16.5 47)        Black ((mkColor 1 1 1)`scaleColor`0.999)  SPEC -- Mirr
  ,Sphere 16.5 (mkPosition 73 16.5 78)        Black ((mkColor 1 1 1)`scaleColor`0.999)  REFR -- Glas
  ,Sphere 1.5  (mkPosition 50 (81.6-16.5) 81.6) ((mkColor 4 4 4)`scaleColor`100)   Black  DIFF -- Lite
  ]

lights :: [Sphere]
lights = filter (not . isBlack . emission) spheres

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

radianceDiffuse :: Gen RealWorld -> Ray -> Int -> IO Color
radianceDiffuse gen r depth = radiance gen r depth 1
radianceSpecular :: Gen RealWorld -> Ray -> Int -> IO Color
radianceSpecular gen r depth = radiance gen r depth 0

arbitraryOrthogonal :: Direction k -> Direction 'NotNormalized
arbitraryOrthogonal (Direction d@(Vec x _ _))
  | abs x > 0.1 = Direction (Vec 0 1 0 .%. d)
  | otherwise = Direction (Vec 1 0 0 .%. d)

getDiffuseDirect :: Sample2D -> Position -> Direction 'NotNormalized -> Color
getDiffuseDirect (Sample2D eps1 eps2) x nl = foldl' addColor Black (map getALightContrib lights)
  where
    getALightContrib :: Sphere -> Color
    getALightContrib s@(Sphere{..}) = do
          let sw = directionFromTo x position
              su = normalize (arbitraryOrthogonal sw)
              sv = sw `orthogonalDirection` su
              cos_a_max = sqrt (1 - radius ^ (2 :: Int) / (norm2 (directionFromTo position x)))

              cos_a = 1 - eps1 + eps1 * cos_a_max
              sin_a = sqrt (1 - cos_a * cos_a)
              phi = 2 * pi * eps2
              l = normalize $ (rotateBasis (su, sv, sw) (mkDirection
                                                          (cos phi * sin_a)
                                                          (sin phi * sin_a)
                                                          cos_a))


              -- this is silly, but, well ;)
              itSphere = intersectSphere (Ray x l) s
          case (itSphere, intersectScene (Ray x l)) of
            (Just it, Just it') ->  if (getT it) == (getT it')
                        then let omega = 2 * pi * (1 - cos_a_max)
                             in (emission `scaleColor` (l `cosinus` nl * omega / pi))
                        else Black
            _ -> Black

mixColor :: Color -> Color -> Color
mixColor (Color c) (Color c') = Color (c .*. c')

addColor :: Color -> Color -> Color
addColor (Color c) (Color c') = Color (c .+. c')

getDiffuseIndirect :: Sample2D -> Direction 'NotNormalized -> Position -> (Ray, Double)
getDiffuseIndirect (Sample2D su r2) nl x =
                  let pi2 = (2 * pi) :: Double
                      r1 = pi2 * su
                      r2s = sqrt r2
                      w = nl
                      u = normalize (arbitraryOrthogonal w)
                      v = w `orthogonalDirection` u
                      d = normalize $ rotateBasis (u, v, w) (mkDirection
                                                             (cos r1 * r2s)
                                                             (sin r1 * r2s)
                                                             (sqrt (1 - r2)))

                  in ((Ray x d), 1)

reflect :: Direction 'NotNormalized -> Direction 'NotNormalized -> Direction 'NotNormalized
reflect (Direction direction) (Direction n) = Direction (direction .-. (n .* (2 * n `dot` (direction))))

getSpecularIndirect :: Position -> Direction 'NotNormalized -> Direction 'NotNormalized -> IO (Ray, Double)
getSpecularIndirect x direction n = pure (Ray x (reflect direction n), 1)

minus :: Direction k -> Direction k' -> Direction 'NotNormalized
minus (Direction d) (Direction d') = Direction (d .-. d')

getRefractionIndirect :: Sample2D -> Direction 'NotNormalized -> Position -> Direction 'NotNormalized -> Direction 'NotNormalized -> (Ray, Double)
getRefractionIndirect (Sample2D rnd _) nl x direction n =
                  let reflRay = Ray x (reflect direction n)
                      into = n `cosinus` nl > 0
                      nc = 1
                      nt = 1.5
                      nnt = if into then (nc / nt) else (nt / nc)
                      ddn = direction `cosinus` nl
                      cos2t = 1 - nnt * nnt * (1 - ddn * ddn)

                  in if cos2t < 0
                    then (reflRay, 1)
                    else do
                      let tdir = normalize ((scale direction nnt) `minus` (scale nl ((ddn * nnt + sqrt cos2t))))
                          a = nt - nc
                          b = nt + nc
                          r0 = (a / b) ^ (2 :: Int)
                          c = 1 - if into then -ddn else (tdir `cosinus` n)
                          re = r0 + (1-r0) * (c ^ (5 :: Int))
                          tr = 1 - re
                          p'' = 0.25 + 0.5 * re
                          rp = re / p''
                          tp = tr / (1 - p'')

                      if rnd < p''
                        then (reflRay, rp)
                        else (Ray x tdir, tp)

reflectBSDF :: Gen RealWorld -> Sphere -> Direction 'NotNormalized -> Position -> Int -> Direction 'NotNormalized -> Direction 'NotNormalized -> Color -> IO Color
reflectBSDF gen obj nl x depth direction n f = do
              sample <- sample2D gen
              case refl obj of
                DIFF -> do
                  let direct = getDiffuseDirect sample x nl
                      (iRay, iWeight) = getDiffuseIndirect sample nl x

                  indirect <- radiance gen iRay (depth + 1) 1
                  pure (f `mixColor` (direct `addColor` (indirect `scaleColor` iWeight)))
                SPEC -> do
                  (iRay, iWeight) <- getSpecularIndirect x direction n

                  indirect <- radiance gen iRay (depth + 1) 1

                  pure (f `mixColor` indirect `scaleColor` iWeight)
                REFR -> do
                  let (iRay, iWeight) = getRefractionIndirect sample nl x direction n

                  indirect <- radiance gen iRay (depth + 1) 1

                  pure (f `mixColor` indirect `scaleColor` iWeight)

scale :: Direction k -> Double -> Direction 'NotNormalized
scale (Direction d) f = Direction (d .* f)

reverseDirection :: Direction k -> Direction k
reverseDirection (Direction d) = Direction (d .* (-1))

maxComp :: Color -> Double
maxComp (Color (Vec r g b)) = max r (max g b)

radiance :: Gen RealWorld -> Ray -> Int -> Int -> IO Color
radiance gen r@Ray{..} depth e' = do
  case intersectScene r of
    Nothing -> pure Black
    Just (Intersect obj t) -> do
      let x = origin `translate` (scale direction t)
          n = normalize (directionFromTo (position obj) x)
          nl = if direction `cosinus` n < 0 then n else reverseDirection n
          f = color obj
          p' = maxComp f

          ref = reflectBSDF gen obj nl x depth direction n
      if (depth + 1) > 5 || p' == 0
        then do
          rv <- uniform gen
          if rv < p'
            then do
              res <- ref (f `scaleColor` (1 / p'))
              pure (emission obj `scaleColor` (fromIntegral e') `addColor` res)
            else pure (emission obj `scaleColor` (fromIntegral e'))
        else do
           res <- ref f
           pure (emission obj `scaleColor` (fromIntegral e') `addColor` res)

mkDirection :: Double -> Double -> Double -> Direction 'NotNormalized
mkDirection a b c = Direction (Vec a b c)

orthogonalDirection :: Direction k -> Direction k' -> Direction 'NotNormalized
orthogonalDirection (Direction a) (Direction b) = Direction (a .%. b)

rotateBasis :: (Direction k, Direction k, Direction k) -> Direction k -> Direction k
rotateBasis (Direction bx, Direction by, Direction bz) (Direction (Vec x y z)) = Direction (bx .* x .+. by .* y .+. bz .* z)

main :: IO ()
main = do
  args <- getArgs

  let w = 1024 :: Int
      h = 768 :: Int
      samps = maybe 1 (`div`4) (listToMaybe args >>= readMaybe) :: Int
      cam = Ray (mkPosition 50 52 295.6) (normalize (mkDirection 0 (-0.042612) (-1)))
      cx = mkDirection (fromIntegral w * 0.5135 / fromIntegral h) 0 0
      cy = (normalize (cx `orthogonalDirection` direction cam)) `scale` 0.5135

      (output) = if length args == 2
                 then Just (args !! 1)
                 else Nothing

  gen <- create

  c <- MV.replicate (h * w) Black
  forConcurrently_ [0 .. (h - 1)] $ \y -> do
    when (isJust output) $ do
      putStr ("\rRendering (" ++ show (samps * 4) ++ " spp) " ++ show ((100 * fromIntegral y / (fromIntegral h - 1)) :: Double) ++ "%.")
      hFlush stdout
    for_ [0..(w - 1)] $ \x -> do
       let i = (h - y - 1) * w + x
       for_ [0..1] $ \sy ->
          for_ [0..1] $ \sx -> do
             let fFold accum _ = do
                         (Sample2D u v) <- sample2D gen
                         let r1 = 2 * u
                             r2 = 2 * v
                             dx = if r1 < 1 then sqrt(r1) -1 else 1 - sqrt(2 - r1)
                             dy = if r2 < 1 then sqrt(r2) -1 else 1 - sqrt(2 - r2)

                             d' = rotateBasis (cx, cy, direction cam) (mkDirection
                                                                        ((((sx + 0.5 + dx) / 2 + fromIntegral x) / fromIntegral w - 0.5))
                                                                        ((((sy + 0.5 + dy) / 2 + fromIntegral y) / fromIntegral h - 0.5))
                                                                        1)

                         rad <- radiance gen (Ray (origin cam `translate` (scale d' 140)) (normalize d')) 0 1
                         pure $ accum `addColor` rad
             rad <- foldlM fFold Black [0..(samps - 1)]
             let clampedRes = clampV rad `scaleColor` 0.25

             old <- MV.unsafeRead c i
             MV.unsafeWrite c i $! (old `addColor` clampedRes)

  case output of
    Nothing -> pure ()
    Just filename -> do
      let header = ("P3\n" ++ show w ++ " " ++ show h ++ "\n255\n")

      withFile filename WriteMode $ \handle -> do
        hPutStr handle header
        for_ [0..(w * h) - 1] $ \i -> do
          v <- MV.unsafeRead c i
          hPutStr handle (v2c v)

translate :: Position -> Direction k -> Position
translate (Position p) (Direction d) = Position (p .+. d)

v2c :: Color -> [Char]
v2c (Color (Vec a b c)) = show (toInt a) ++ " " ++ show (toInt b) ++ " " ++ show (toInt c) ++ " "

clampV :: Color -> Color
clampV (Color (Vec a b c)) = Color (Vec (clamp a) (clamp b) (clamp c))
