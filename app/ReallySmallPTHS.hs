-- Haskell 2020?
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- 40s versus 7s par 4spp at 1024x768 ;)
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS -Wall #-}

module Main where

import Data.Foldable (for_)
import System.IO (withFile, IOMode(..), hPutStr)
import Data.Foldable (foldl')
import Data.Traversable (for)
import GHC.Prim (RealWorld)
import Control.Concurrent.Async
import Options.Generic
import Control.Monad (replicateM)

import System.Random.MWC (create, uniform, Gen)

import Linear.V3
import Linear.Matrix
import Linear.Metric
import Linear.Vector

-- Type aliases to help readability during the talk
-- In real life, I'll use newtype and smart deriving of the important operations
type Position = V3 Double
type Direction = V3 Double
type Normal = V3 Double
type Color = V3 Double

-- | A ray
data Ray
  = Ray
  { origin :: !Position
  , direction :: !Direction
  , time :: !Double
  } deriving (Show)
-- P = origin + t * direction

data Sphere
  = Sphere
  { radius :: !Double
  , center :: !Position
  } deriving (Show, Eq)
-- (center - P) ^ 2 = R^2

-- | A bit of floating point offset
-- | Demo: with and without this ?
epsilon :: Double
epsilon = 0.001

-- | Return the first positive ray abscise where an intersection with
--   the sphere is found
intersectSphere :: Ray -> Sphere -> Maybe Double
intersectSphere Ray{origin, direction} Sphere{center, radius} =
  -- Ray: P = origin + t * direction
  -- Sphere: dot2 (center - P) = radius ^ 2
  -- ==> dot2 (center - origin - t * direction) = radius ^ 2
  let
    op = center - origin
    -- ==>
    --   (opX - (t * directionX)) ^ 2
    -- + (opY - (t * directionY)) ^ 2
    -- + (opZ - (t * directionZ)) ^ 2
    -- - radius ^ 2 == 0
    --
    -- =>    t ^ 2 * dot2 direction
    --    - 2 * t * dot op direction
    --    + dot2 op - R^2 == 0

    -- we know that dot2 direction == 1 by construction (normalized ray direction)
    -- Then we solve the second order equation where
    -- A = 1
    -- B = - 2 dot op direction = - 2 b
    b = dot op direction
    -- C = dot2 op - R ^ 2

    -- Delta = B ^ 2 - 4 (AC)
    --       = 4 b ^ 2 - 4 C
    --       = 4 * det'
    det' = b * b - (dot op op - radius * radius)

    -- Solutions = [- B +- sqrt (Delta)] / (2 * A)
    --           = [- B / 2 +- sqrt (Delta / 4)]
    --           = [- B / 2 +- sqrt (Delta / 4)]
    --           = [ b +- sqrt det]
    det = sqrt det'
    ta = b - det
    tb = b + det
  in
    if
      | det' < 0 -> Nothing
      | ta > epsilon -> Just ta
      | tb > epsilon -> Just tb
      | otherwise -> Nothing


-- * Real raytracing stuffs starts now
-- | Basic material model
data MaterialModel
  = Diffuse
  | Mirror
  | Glass
  deriving (Show, Eq)

data Object
  = Object
  { shape :: Sphere
  , emission :: !Color
  , color :: !Color
  , refl :: !MaterialModel
  }
  deriving (Show)

-- | The scene !
spheres :: Double -> [Object]
spheres time = [
   Object (Sphere 1e5  (V3  (1e5+1) 40.8 81.6))  Black               (V3 0.75 0.25 0.25)   Diffuse -- Left
  ,Object (Sphere 1e5  (V3 (-1e5+99) 40.8 81.6)) Black               (V3 0.25 0.25 0.75)   Diffuse -- Right
  ,Object (Sphere 1e5  (V3 50 40.8  1e5))        Black               (V3 0.75 0.75 0.75)   Diffuse -- Back
  ,Object (Sphere 1e5  (V3 50 40.8 (-1e5+170)))  Black               Black                 Diffuse -- Front
  ,Object (Sphere 1e5  (V3 50  1e5  81.6))       Black               (V3 0.75 0.75 0.75)   Diffuse -- Bottom
  ,Object (Sphere 1e5  (V3 50 (-1e5+81.6) 81.6)) Black               (V3 0.75 0.75 0.75)   Diffuse -- Top
  ,Object (Sphere 16.5 (lerp time (V3 27 16.5 47) (V3 40 25 55)))          Black               ((V3 1 1 1) ^* 0.999) Mirror -- Mirror
  ,Object (Sphere 16.5 (V3 73 16.5 78))          Black               ((V3 1 1 1) ^* 0.999) Glass -- Glass
  ,Object (Sphere 1.5  (V3 50 (81.6-16.5) 81.6)) ((V3 4 4 4) ^* 100) Black                 Diffuse -- Light
  ]


-- | List of object which are lights
lights :: Double -> [Object]
lights t = filter (not . isBlack . emission) (spheres t)

-- | The first object (if any) on the ray path
intersectScene :: Ray -> Maybe (Object, Double)
intersectScene ray = foldl' findIntersect Nothing (spheres (time ray))
  where
    findIntersect Nothing o = (o, ) <$> intersectSphere ray (shape o)
    findIntersect (old@(Just ((_, oldT)))) o = case intersectSphere ray (shape o) of
      Nothing -> old
      Just newT -> if newT < oldT
        then Just (o,newT)
        else old

data IntersectionFrame =
  IntersectionFrame
  { x :: Position
  -- ^ intersection position
  , wi :: Direction
  -- ^ ray direction, toward surface
  , normal :: Normal
  -- ^ Pointing on the right side
  , outside :: Bool
  -- ^ are we outside the surface
  , timeFrame :: Double
  } deriving (Show)

intersectFrame :: Ray -> Maybe (Object, IntersectionFrame)
intersectFrame r@Ray{origin, direction, time} = do
  (obj, t) <- intersectScene r
  let
    x = origin + t *^ direction
    wi = direction
    normalWrongSide = normalize (x - (center (shape obj))) -- Normal
    outside = direction `dot` normalWrongSide < 0
    normal = if outside then normalWrongSide else -normalWrongSide -- Normal oriented on the right side

  Just (obj, IntersectionFrame{x, wi, normal, outside, timeFrame=time})

radiance :: Gen RealWorld -> Ray -> Int -> Bool -> IO Color
radiance gen ray depth useEmit = do
  case intersectFrame ray of
    Nothing -> pure Black
    Just (obj, itFrame) -> do
      let
        f = color obj
        p' = maximum f

        emit = if useEmit && depth < 2
               then emission obj
               else Black

      -- Russian rulette, to continue or not
      rv <- sample1D gen
      if rv > p'
        then pure emit
        else do
          sample <- sample2D gen

          let
            direct = case refl obj of
              Diffuse -> getDiffuseDirect sample itFrame
              _ -> Black

            indirectRay = case refl obj of
              Diffuse -> getDiffuseIndirect sample itFrame
              Mirror -> getMirrorIndirect itFrame
              Glass -> getGlassIndirect sample itFrame

          indirect <- radiance gen indirectRay (depth + 1) (refl obj /= Diffuse)

          pure (emit + (1 / p') *^ (f  * (direct + indirect)))

-- Direct Lighting

-- Main

data Options = Options
  { outputFile :: FilePath
  , width :: Int
  , height :: Int
  , samples :: Int
  , enableMotionBlur :: Bool
  } deriving (Generic, Show, ParseRecord)

timeStrategy :: Gen RealWorld -> Bool -> IO Double
timeStrategy gen enableMotionBlur
  | enableMotionBlur = sample1D gen
  | otherwise = pure 1

main :: IO ()
main = do
  Options{outputFile, width, height, samples, enableMotionBlur} <- getRecord "Simple Ray Tracer"

  let
    sampleCam = sampleCamera (V3 50 52 295.6) (normalize (V3 0 (-0.042612) (-1))) (width, height)

  gen <- create

  pixels <- forConcurrently [1..height] $ \y -> do
    for [0..(width-1)] $ \x -> do
      contributions <- replicateM samples $ do
        cameraRay <- sampleCam (height - y, x) <$> sample2D gen <*> timeStrategy gen enableMotionBlur
        radiance gen cameraRay 0 True

      -- sum and normalize contributions
      -- HEY: I had a space leak here!
      pure $! foldl' (+) Black contributions ^/ fromIntegral samples

  withFile outputFile WriteMode $ \handle -> do
    hPutStr handle ("P3\n" ++ show width ++ " " ++ show height ++ "\n255\n")
    for_ (concat pixels) $ \v -> do
      hPutStr handle (v2c v)

-- | Compute direct lighting at a point
getDiffuseDirect :: Sample2D -> IntersectionFrame -> Color
getDiffuseDirect sample2d IntersectionFrame{x, normal, timeFrame} = foldl' (+) Black (map getALightContrib (lights timeFrame)) -- sum of all light contributions
  where
    getALightContrib :: Object -> Color
    getALightContrib Object{emission, shape=s@(Sphere{center, radius})} = do
          let
            -- sample a direction to the light
            xp = center - x
            cos_a_max = sqrt (1 - radius ^ (2 :: Int) / (dot xp xp))
            l = sampleCosinusMax sample2d cos_a_max `rotateAround` (normalize xp)

          case intersectScene (Ray x l timeFrame) of
            -- This is a bit contrived. If the only intersection we
            -- find is the light itself, it means that there is no
            -- intersection on the path

            -- It is correct because we generate a ray toward the
            -- sphere and it is a convexe object.
            -- However this won't be correct in a real life scenario
            -- because light won't always be convexe
            Just (obj, _) ->  if shape obj  == s
                        then let omega = 2 * pi * (1 - cos_a_max)
                             in (emission ^* (l `dot` normal * omega / pi))
                        -- the full equation is as following:
                        -- surface_bsdf: (l . nl / pi) *
                        -- light_contribution: emission * form_factor
                        -- / squared distance
                        -- / probability density function
                        -- However the pdf contains a few part:
                        -- direction sampling: 1 / omega
                        -- light_form_factor
                        -- squared distance
                        -- many terms cancels, resulting in the above equation
                        else Black
            _ -> Black

-- | Returns the mirror indirect direction, with a scaling factor
getMirrorIndirect :: IntersectionFrame -> Ray
getMirrorIndirect IntersectionFrame{x, wi, normal, timeFrame} = Ray x (reflect wi normal) timeFrame
-- Here the scaling factor is equal to one, there is no sampling.

-- | Returns the diffuse indirect direction, with a scaling factor
getDiffuseIndirect :: Sample2D -> IntersectionFrame -> Ray
getDiffuseIndirect sample IntersectionFrame{normal, x, timeFrame} = Ray x (sampleCosinus sample `rotateAround` normal) timeFrame
-- Here the scaling factor is equal to the cosinus contribution of the
-- surface divided by the density probability, which is the same (we
-- did that on purpose ;) So 1 ;)

-- | Returns the glass indirect direction, with a scaling factor
getGlassIndirect :: Sample2D -> IntersectionFrame -> Ray
getGlassIndirect (Sample2D rnd _) IntersectionFrame{wi, normal, outside, x, timeFrame} =
  let
    reflRay = Ray x (reflect wi normal) timeFrame
    ior = 1.5
  in case refract wi normal outside ior of
    Nothing -> reflRay
    Just (transmitCoef, transmitDir) -> let
      in if rnd < transmitCoef
        then Ray x transmitDir timeFrame
        else reflRay

sampleCamera :: Position -> Direction -> (Int, Int) -> (Int, Int) -> Sample2D -> Double -> Ray
sampleCamera focalPoint viewDirection (width, height) (y, x) (Sample2D u v) time = let
  cx = V3 (fromIntegral width * 0.5135 / fromIntegral height) 0 0
  cy = (normalize (cx `cross` viewDirection)) ^* 0.5135

  -- Box sampling of the pixel neighboorhood
  dx = 2 * u - 1
  dy = 2 * v - 1

  cameraBase = V3 cx cy viewDirection

  d' = (V3
         ((dx + fromIntegral x) / fromIntegral width - 0.5)
         ((dy + fromIntegral y) / fromIntegral height - 0.5)
         1) *! cameraBase

  in Ray (focalPoint + (d' ^* 140)) (normalize d') time

v2c :: Color -> [Char]
v2c ((V3 a b c)) = show (tonemap a) ++ " " ++ show (tonemap b) ++ " " ++ show (tonemap c) ++ " "

tonemap :: Double -> Int
tonemap x = truncate (((clamp x) ** (1 / 2.2)) * 255 + 0.5)

-- | 'reflect wi n' returns the reflected direction
--   wi is the incoming direction, toward the surface
--   n is the normal, going out of the surface
reflect :: Direction -> Normal -> Direction
reflect direction n = direction - (n ^* (2 * n `dot` (direction)))

refract :: Direction -> Normal -> Bool -> Double -> Maybe (Double, Direction)
refract direction nl outside ior' = let
  -- Here we compute the refraction angle
  -- Do to fresnel laws, total internal reflection may happen, hence no refraction
  -- n_1 sin t_1 = n_2 sin t_2
  -- ior = n1 / n2
  ior = if outside then 1 / ior' else ior'

  cos_t1 = direction `dot` nl
  cos_t2_2 = 1 - ior * ior * (1 - cos_t1 * cos_t1)

  in if cos_t2_2 < 0
    then Nothing
    else let
      tdir = normalize ((direction ^* ior) - (nl ^* ((cos_t1 * ior + sqrt cos_t2_2))))

      -- Schlick approximation of Fresnel coefficient for reflection coefficient
      r0 = ((ior - 1) / (ior + 1)) ^ (2 :: Int)

      -- cosTheta must be the angle toward the light
      -- I don't understand smallpt here which uses the following:
      cosTheta = 1 - if outside then -cos_t1 else (abs $ tdir `dot` nl)
      -- cosTheta = 1 + cos_t1
      reflectCoef = r0 + (1-r0) * (cosTheta ^ (5 :: Int))
      in Just (1 - reflectCoef, tdir)

-- * Geometry utils

-- | @d `rotateAround` z@ returns a new vector which represents @d@
--   positioned in an arbitrary basis formed around axis @z@
--   Note: the output direction is normalized only if the input base
--   and direction are orthogonal and normalized
rotateAround :: Direction -> Direction -> Direction
rotateAround d z =
  let
    u = arbitraryOrthogonal z
    v = z `cross` u
  in
    d *! V3 u v z

-- | Returns an arbitrary normalized direction in orthogonal plane
arbitraryOrthogonal :: Direction -> Direction
arbitraryOrthogonal (d@(V3 x _ _)) =
  normalize $ if
  | abs x > 0.1 -> V3 0 1 0 `cross` d
  | otherwise -> V3 1 0 0 `cross` d

-- * Colors utils

pattern Black :: Color
pattern Black = V3 0 0 0

isBlack :: Color -> Bool
isBlack Black = True
isBlack _ = False

-- * Maths utils

clamp :: (Num a, Ord a) => a -> a
clamp x = min 1 (max 0 x)

-- Sampling

data Sample2D = Sample2D !Double !Double
  deriving (Show)

sample1D :: Gen RealWorld -> IO Double
sample1D = uniform

sample2D :: Gen RealWorld -> IO Sample2D
sample2D gen = Sample2D <$> uniform gen <*> uniform gen


-- | Sample a direction in hemisphere proportional to solid angle
sampleCosinus :: Sample2D -> Direction
sampleCosinus (Sample2D su r2) =
  let tau = 2 * pi
      r1 = tau * su
      r2s = sqrt r2
  in V3 (cos r1 * r2s) (sin r1 * r2s) (sqrt (1 - r2))

-- | Sample a direction in hemisphere proportional to solid angle in a clamped spherical cap
sampleCosinusMax
  :: Sample2D
  -> Double -- ^ cosinus of the spherical cap
  -> Direction
sampleCosinusMax (Sample2D r1 r2) cos_max =
  let
    cos_a = 1 - r2 + r2 * cos_max
    sin_a = sqrt (1 - cos_a * cos_a)
    phi = 2 * pi * r1
  in V3
     (cos phi * sin_a)
     (sin phi * sin_a)
     cos_a
