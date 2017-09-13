{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedSums #-}

module Main where

import Data.Maybe (listToMaybe, isJust)
import Data.Foldable (for_)
import System.IO (hFlush, stdout, withFile, IOMode(..), hPutStr)
import System.Random.MWC (create, uniform, Gen)
import Text.Read (readMaybe)
import Data.Foldable (foldlM, foldl')
import System.Environment (getArgs)
import GHC.Prim (RealWorld)
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Concurrent.Async
import Control.Monad (when)
import Data.Vector.Unboxed.Deriving
import Control.Exception.Base (assert)

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

dot :: Vec -> Vec -> Double
dot (Vec a b c) (Vec a' b' c') = a * a' + b * b' + c * c'

----------------------------------------------------------
-- Vec public API (Position, Direction, Color)
----------------------------------------------------------

-- | A point in Space
newtype Position = Position Vec deriving (Show)


-- | A direction, can be normalized or not
newtype Direction (k :: NormalizedStatus) = Direction Vec deriving (Show)
data NormalizedStatus = NotNormalized | Normalized

-- | A color
newtype Color = Color Vec deriving (Show)

-- which can be unboxed for the image storage
derivingUnbox "Color"
    [t| Color -> (Double, Double, Double) |]
    [| \ (Color (Vec a b c)) -> (a, b, c) |]
    [| \ (a, b, c) -> (Color (Vec a b c)) |]

-- Shitload of function on Position / Direction / Color
-- I tried to be as typesafe as possible. This is more complicated
-- than the original smallpt.cpp, but gives something more readable / safe
-- (that's my opinion)

-- Direction and Positions

-- | Normalize a direction
normalize :: Direction 'NotNormalized -> Direction 'Normalized
normalize (Direction(v@(Vec a b c))) = Direction (v .* (1 / norm))
  where norm = sqrt (a * a + b * b + c * c)

-- | Create the direction AB from A and B
directionFromTo :: Position -> Position -> Direction 'NotNormalized
directionFromTo (Position from) (Position to) = Direction (to .-. from)

-- | Project a direction onto another
project :: Direction k -> Direction k' -> Double
project (Direction d) (Direction nd) = d `dot` nd

-- | Cosinus of the angle between two normalized direction
cosinus :: Direction 'Normalized -> Direction 'Normalized -> Double
cosinus (Direction d) (Direction nd) = d `dot` nd

-- | Squared norm
norm2 :: Direction 'NotNormalized -> Double
norm2 (Direction d) = d `dot` d

mkPosition :: Double -> Double -> Double -> Position
mkPosition x y z = Position (Vec x y z)

mkColor :: Double -> Double -> Double -> Color
mkColor x y z = Color (Vec x y z)

mkDirection :: Double -> Double -> Double -> Direction 'NotNormalized
mkDirection a b c = Direction (Vec a b c)

minus :: Direction k -> Direction k' -> Direction 'NotNormalized
minus (Direction d) (Direction d') = Direction (d .-. d')

scale :: Direction k -> Double -> Direction 'NotNormalized
scale (Direction d) f = Direction (d .* f)

reverseDirection :: Direction k -> Direction k
reverseDirection (Direction d) = Direction (d .* (-1))

translate :: Position -> Direction k -> Position
translate (Position p) (Direction d) = Position (p .+. d)

-- Basis

-- | Return an orthogonal direction between two directions
orthogonalDirection :: Direction k -> Direction k' -> Direction 'NotNormalized
orthogonalDirection (Direction a) (Direction b) = Direction (a .%. b)

-- | `rotateBasis (x, y, z) v` rotates the vector `v` to the new basis `(x, y, z)`
-- | (x,y,z) are assumed to be orthogonals
rotateBasis :: (Direction 'Normalized, Direction 'Normalized, Direction 'Normalized) -> Direction 'Normalized -> Direction 'Normalized
rotateBasis (Direction bx, Direction by, Direction bz) (Direction (Vec x y z)) = Direction (bx .* x .+. by .* y .+. bz .* z)

rotateBasisUnsafe :: (Direction k, Direction k', Direction k'') -> Direction k''' -> Direction 'NotNormalized
rotateBasisUnsafe (Direction bx, Direction by, Direction bz) (Direction (Vec x y z)) = Direction (bx .* x .+. by .* y .+. bz .* z)

rotateAround :: Direction 'Normalized -> Direction 'Normalized -> Direction 'Normalized
rotateAround w d =
  let
    u = normalize (arbitraryOrthogonal w)
    v = w `orthogonalDirection` u
  in
    rotateBasis (u, asNormalized v, w) d

mkDirectionNormalized :: Double -> Double -> Double -> Direction 'Normalized
mkDirectionNormalized x y z = asNormalized (mkDirection x y z)

asNormalized :: Direction k -> Direction 'Normalized
asNormalized (Direction v) = assert (v `dot` v == 1) (Direction v)

arbitraryOrthogonal :: Direction k -> Direction 'NotNormalized
arbitraryOrthogonal (Direction d@(Vec x _ _))
  | abs x > 0.1 = Direction (Vec 0 1 0 .%. d)
  | otherwise = Direction (Vec 1 0 0 .%. d)

-- Colors

isBlack :: Color -> Bool
isBlack Black = True
isBlack _ = False

scaleColor :: Color -> Double -> Color
scaleColor (Color c) f = Color (c .* f)

clamp :: (Num a, Ord a) => a -> a
clamp x = min 1 (max 0 x)

toInt :: Double -> Int
toInt x = truncate (((clamp x) ** (1 / 2.2)) * 255 + 0.5)

mulColor :: Color -> Color -> Color
mulColor (Color c) (Color c') = Color (c .*. c')

addColor :: Color -> Color -> Color
addColor (Color c) (Color c') = Color (c .+. c')

-- | returns the maximum component of a color
maxComp :: Color -> Double
maxComp (Color (Vec r g b)) = max r (max g b)

clampV :: Color -> Color
clampV (Color (Vec a b c)) = Color (Vec (clamp a) (clamp b) (clamp c))


--- Sampling
data Sample2D = Sample2D !Double !Double
  deriving (Show)

sample2D :: Gen RealWorld -> IO Sample2D
sample2D gen = Sample2D <$> uniform gen <*> uniform gen

sample1D :: Gen RealWorld -> IO Double
sample1D = uniform

---
-- Real raytracing stuffs stats now
--

-- | A ray
data Ray = Ray {origin :: !Position, direction :: !(Direction 'Normalized)} deriving (Show)

-- | Basic material model
data MaterialModel = Diffuse
            | Mirror
            | Glass
            deriving (Show)

data Sphere = Sphere {
  radius :: !Double,
  position :: !Position, -- ^ Center of the sphere
  emission :: !Color,
  color :: !Color,
  refl :: !MaterialModel
  } deriving (Show)


data Intersect = Intersect {
  getObj :: !Sphere, -- ^ The intersected object
  getT :: !Double -- ^ The intersection distance
  } deriving (Show)

type MaybeIntersect = (# Intersect | () #)

intersectSphere :: Ray -> Sphere -> MaybeIntersect
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
      | det' < 0 -> (#| () #)
      | ta > eps -> (# Intersect s ta | #)
      | tb > eps -> (# Intersect s tb | #)
      | otherwise -> (#| () #)

spheres :: [Sphere]
spheres = [
   Sphere 1e5  (mkPosition  (1e5+1) 40.8 81.6)  Black (mkColor 0.75 0.25 0.25) Diffuse -- Left
  ,Sphere 1e5  (mkPosition (-1e5+99) 40.8 81.6) Black (mkColor 0.25 0.25 0.75) Diffuse -- Rght
  ,Sphere 1e5  (mkPosition 50 40.8  1e5)      Black (mkColor 0.75 0.75 0.75) Diffuse -- Back
  ,Sphere 1e5  (mkPosition 50 40.8 (-1e5+170))  Black Black            Diffuse -- Frnt
  ,Sphere 1e5  (mkPosition 50  1e5  81.6)     Black (mkColor 0.75 0.75 0.75) Diffuse -- Botm
  ,Sphere 1e5  (mkPosition 50 (-1e5+81.6) 81.6) Black (mkColor 0.75 0.75 0.75) Diffuse -- Top
  ,Sphere 16.5 (mkPosition 27 16.5 47)        Black ((mkColor 1 1 1)`scaleColor`0.999)  Mirror -- Mirr
  ,Sphere 16.5 (mkPosition 73 16.5 78)        Black ((mkColor 1 1 1)`scaleColor`0.999)  Glass -- Glas
  ,Sphere 1.5  (mkPosition 50 (81.6-16.5) 81.6) ((mkColor 4 4 4)`scaleColor`100)   Black  Diffuse -- Lite
  ]

lights :: [Sphere]
lights = filter (not . isBlack . emission) spheres

myfoldl' :: (MaybeIntersect -> Sphere -> MaybeIntersect) -> MaybeIntersect -> [Sphere] -> MaybeIntersect
myfoldl' f = go
  where go acc [] = acc
        go acc (x:xs) = go (f acc x) xs

intersectScene :: Ray -> MaybeIntersect
intersectScene ray = myfoldl' findIntersect (# | () #) spheres
  where
    findIntersect (#| () #) s = intersectSphere ray s
    findIntersect (old@((#Intersect _ oldT| #))) s = case intersectSphere ray s of
      (#| () #) -> old
      new@((#(Intersect _ newT)| #)) -> if newT < oldT
        then new
        else old

-- | `reflect wi n` returns the reflected direction
-- TODO: find a way to represent this API without a risk of swapping
-- both direction. For example, introduce a new type for Normal
reflect :: Direction 'Normalized -> Direction 'Normalized -> Direction 'Normalized
reflect (Direction direction) (Direction n) = Direction (direction .-. (n .* (2 * n `dot` (direction))))

-- Direct Lighting

-- | Compute direct lighting at a point
getDiffuseDirect :: Sample2D -> Position -> Direction 'Normalized -> Color
getDiffuseDirect (Sample2D eps1 eps2) x nl = foldl' addColor Black (map getALightContrib lights) -- sum of all light contributions
  where
    getALightContrib :: Sphere -> Color
    getALightContrib s@(Sphere{..}) = do
          -- This especially contrived code generates a sampling
          -- direction on point `position` and on the direction of the
          -- lighting sphere

          -- it generates the direction with a probability density function with respect to position
          -- which cancels with the final contribution
          -- Seriously, I should check thoses maths
          let sw = directionFromTo x position
              su = normalize (arbitraryOrthogonal sw)
              sv = sw `orthogonalDirection` su
              cos_a_max = sqrt (1 - radius ^ (2 :: Int) / (norm2 (directionFromTo position x)))

              cos_a = 1 - eps1 + eps1 * cos_a_max
              sin_a = sqrt (1 - cos_a * cos_a)
              phi = 2 * pi * eps2
              l = normalize $ (rotateBasisUnsafe (su, sv, sw) (mkDirection
                                                          (cos phi * sin_a)
                                                          (sin phi * sin_a)
                                                          cos_a))


              -- this is silly, but, well, we compute the intersection with the lighting sphere
              -- and compare it with the intersection in the scene
              -- if they are the same, well, there is not object closer than the lighting sphere

              -- TODO: smarter intersection primitive with early exit
              itSphere = intersectSphere (Ray x l) s
          case (# itSphere, intersectScene (Ray x l) #) of
            (# (# it | #), (# it' | #) #) ->  if (getT it) == (getT it')
                        then let omega = 2 * pi * (1 - cos_a_max)
                             in (emission `scaleColor` (l `cosinus` nl * omega / pi))
                        else Black
            _ -> Black

-- | Returns the mirror indirect direction, with a scaling factor
getMirrorIndirect :: Position -> Direction 'Normalized -> Direction 'Normalized -> IO (Ray, Double)
getMirrorIndirect x direction n = pure (Ray x (reflect direction n), 1)
-- Here the scaling factor is equal to the density probability, which is 1 (well, there is no density probability)

sampleCosinus :: Sample2D -> Direction 'Normalized
sampleCosinus (Sample2D su r2) =
  let pi2 = (2 * pi) :: Double
      r1 = pi2 * su
      r2s = sqrt r2
  in mkDirectionNormalized (cos r1 * r2s) (sin r1 * r2s) (sqrt (1 - r2))

-- | Returns the diffuse indirect direction, with a scaling factor
getDiffuseIndirect :: Sample2D -> Direction 'Normalized -> Position -> (Ray, Double)
getDiffuseIndirect sample nl x =
                  let
                      cosD = sampleCosinus sample
                      d = rotateAround nl cosD
                  in ((Ray x d), 1)

-- Here the scaling factor is equal to the cosinus contribution of the
-- surface divided by the density probability, which is the same (we
-- did that on purpose ;) So 1 ;)

-- | Returns the glass indirect direction, with a scaling factor
getGlassIndirect :: Sample2D -> Direction 'Normalized -> Position -> Direction 'Normalized -> Direction 'Normalized -> (Ray, Double)
getGlassIndirect (Sample2D rnd _) nl x direction n =
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
                        else (Ray x tdir, tp) -- Scaling factor is the probability of selecting reflection or refraction

-- | Compute the light at a point, it may eventually generate indirect rays
reflectBSDF :: Gen RealWorld -> Sphere -> Direction 'Normalized -> Position -> Int -> Direction 'Normalized -> Direction 'Normalized -> Color -> IO Color
reflectBSDF gen obj nl x depth direction n f = do
              sample <- sample2D gen
              case refl obj of
                Diffuse -> do
                  let direct = getDiffuseDirect sample x nl
                      (iRay, iWeight) = getDiffuseIndirect sample nl x

                  indirect <- radiance gen iRay (depth + 1) False
                  pure (f `mulColor` (direct `addColor` (indirect `scaleColor` iWeight)))
                Mirror -> do
                  (iRay, iWeight) <- getMirrorIndirect x direction n

                  indirect <- radiance gen iRay (depth + 1) True

                  pure (f `mulColor` indirect `scaleColor` iWeight)
                Glass -> do
                  let (iRay, iWeight) = getGlassIndirect sample nl x direction n

                  indirect <- radiance gen iRay (depth + 1) True

                  pure (f `mulColor` indirect `scaleColor` iWeight)
  -- TODO: refactor everything here. This piece of code should be something like:
  {-
      direct = getDirectLighting material frame
      indirect <- getIndirectLighting material frame

      With material a material description and frame a local frame with informations such as position, incident ray and normals
  -}

radiance :: Gen RealWorld -> Ray -> Int -> Bool -> IO Color
radiance gen r@Ray{..} depth useEmission = do
  case intersectScene r of
    (# | () #) -> pure Black
    (# Intersect obj t | #) -> do
      let x = origin `translate` (scale direction t) -- Intersection position
          n = normalize (directionFromTo (position obj) x) -- Normal
          nl = if direction `cosinus` n < 0 then n else reverseDirection n -- Normal oriented on the right side
          f = color obj
          p' = maxComp f

          ref = reflectBSDF gen obj nl x depth direction n

          emit = if useEmission
                 then emission obj
                 else Black

      -- only RR after depth > 5
      res <- if (depth + 1) > 5 || p' == 0
        then do
          rv <- sample1D gen
          -- Russian rulette, to continue or not
          if rv < p'
            then ref (f `scaleColor` (1 / p'))
            else pure Black
        else ref f

      pure (emit `addColor` res)

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

       -- super sampling in a grid
       -- TODO: this sucks ;)
       for_ [0..1] $ \sy ->
          for_ [0..1] $ \sx -> do
             let fFold accum _ = do
                         (Sample2D u v) <- sample2D gen
                         let r1 = 2 * u
                             r2 = 2 * v
                             dx = if r1 < 1 then sqrt(r1) -1 else 1 - sqrt(2 - r1)
                             dy = if r2 < 1 then sqrt(r2) -1 else 1 - sqrt(2 - r2)

                             d' = rotateBasisUnsafe (cx, cy, direction cam) (mkDirection
                                                                        ((((sx + 0.5 + dx) / 2 + fromIntegral x) / fromIntegral w - 0.5))
                                                                        ((((sy + 0.5 + dy) / 2 + fromIntegral y) / fromIntegral h - 0.5))
                                                                        1)

                         rad <- radiance gen (Ray (origin cam `translate` (scale d' 140)) (normalize d')) 0 True
                         pure $ accum `addColor` (rad `scaleColor` (1 / fromIntegral samps))
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

v2c :: Color -> [Char]
v2c (Color (Vec a b c)) = show (toInt a) ++ " " ++ show (toInt b) ++ " " ++ show (toInt c) ++ " "
