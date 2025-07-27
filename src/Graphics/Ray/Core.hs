module Graphics.Ray.Core 
  ( -- * Numbers
    infinity, degrees
    -- * Vectors and Rays
  , Vec3, Point3, Color, Dim(..), component, reflect, randomUnitVector, randomInUnitDisk, Ray(Ray)
    -- * Intervals
  , Interval, inInterval, midpoint, padInterval
    -- * Boxes
  , Box, fromCorners, boxJoin, boxHull, allCorners, padBox, shiftBox, longestDim, overlapsBox
    -- * Hit Records
  , HitRecord(..)
  ) where

import Linear (V2, V3(V3), dot, quadrance, (*^), (^/))
import System.Random (RandomGen, randomR)
import Control.Monad.State (MonadState, state)
import Control.Applicative (liftA2)
import Data.Maybe (isJust)
import Data.List (foldl1')

-- | Floating-point infinity.
infinity :: Double
infinity = 1/0

-- | Convert an angle from degrees to radians. @degrees x@ means @x@ degrees.
degrees :: Double -> Double
degrees x = x * pi / 180

type Vec3 = V3 Double
type Point3 = V3 Double
type Color = V3 Double

data Dim = X | Y | Z
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Get the X, Y, or Z component of a vector.
component :: Dim -> V3 a -> a
component X (V3 x _ _) = x
component Y (V3 _ y _) = y
component Z (V3 _ _ z) = z

-- [private]
argMax :: Ord a => V3 a -> Dim
argMax (V3 x y z)
  | x > y     = if x > z then X else Z
  | otherwise = if y > z then Y else Z

-- | If @n@ is the normal vector of a mirror, and @v@ is an incoming light ray, then @reflect n v@ is the outgoing light ray.
-- The first argument should be a unit vector, but the second need not be.
reflect :: Vec3 -> Vec3 -> Vec3
reflect normal v = 
  v - 2 * dot normal v *^ normal

-- | Get a random 3-dimensional vector of norm 1.
randomUnitVector :: (RandomGen g, MonadState g m) => m Vec3
randomUnitVector = do
  vec <- state (randomR (-1, 1))
  let q = quadrance vec
  if 1e-8 <= q && q <= 1
    then pure (vec ^/ sqrt q)
    else randomUnitVector

-- | Get a random 2-dimensional vector of norm less than or equal to 1.
randomInUnitDisk :: (RandomGen g, MonadState g m) => m (V2 Double)
randomInUnitDisk = do
  vec <- state (randomR (-1, 1))
  if quadrance vec <= 1
    then pure vec
    else randomInUnitDisk

-- | A ray with an origin and a direction. Points on the ray @Ray orig dir@ are parametrized by @orig + t *^ dir@.
-- There is no expectation that the direction be a unit vector.
data Ray = Ray Point3 Vec3
  deriving (Show)

-- | An interval with a lower bound and an upper bound. 
-- Variously interpreted as a closed interval or an open interval; it doesn't really matter.
type Interval = (Double, Double)

-- [private]
size :: Interval -> Double
size (a, b) = b - a

-- | Test whether a number is in the interval.
inInterval :: Interval -> Double -> Bool
inInterval (tmin, tmax) t = tmin < t && t < tmax

-- | The midpoint of @(a, b)@ is @(a + b) / 2@.
midpoint :: Interval -> Double
midpoint (a, b) = (a + b) / 2

-- | Extend both the lower bound and the upper bound of the interval by the first argument.
padInterval :: Double -> Interval -> Interval
padInterval padding (tmin, tmax) = (tmin - padding, tmax + padding)

-- [private]
isect :: Interval -> Interval -> Maybe Interval
isect (a, b) (c, d) = let
  imin = max a c
  imax = min b d
  in if imin > imax then Nothing else Just (imin, imax)

-- [private]
overlapsInterval :: Interval -> Double -> Double -> Interval
overlapsInterval (tmin, tmax) x d = let
  t0 = (tmin - x) / d
  t1 = (tmax - x) / d
  in if t0 < t1 then (t0, t1) else (t1, t0)

-- | An axis-aligned box; the product of three intervals.
type Box = V3 Interval

-- | Create a box from two opposite corners.
fromCorners :: Point3 -> Point3 -> Box
fromCorners = liftA2 (\x y -> if x < y then (x, y) else (y, x))

-- | The smallest box containing all of the boxes.
boxJoin :: [Box] -> Box
boxJoin = foldl1' (liftA2 (\(min1, max1) (min2, max2) -> (min min1 min2, max max1 max2))) 

-- | The smallest box containing all of the points.
boxHull :: [Point3] -> Box
boxHull pts = let
  coords = sequence pts
  mins = fmap minimum coords
  maxs = fmap maximum coords
  in liftA2 (,) mins maxs

-- | Get a list of all eight corners of a box.
allCorners :: Box -> [Point3]
allCorners (V3 i1 i2 i3) = 
  [ V3 (f1 i1) (f2 i2) (f3 i3) 
  | f1 <- [ fst, snd ], f2 <- [ fst, snd ], f3 <- [ fst, snd ]
  ]

-- | Extend the box by the first argument in all six directions.
padBox :: Double -> Box -> Box
padBox padding = fmap (padInterval padding)

shiftBox :: Vec3 -> Box -> Box
shiftBox = liftA2 (\x (a, b) -> (a + x, b + x))

-- | The dimension in which the box is the longest.
longestDim :: Box -> Dim
longestDim = argMax . fmap size

-- | Test whether any part of the ray, when restricted to the interval, is within the box.
overlapsBox :: Box -> Ray -> Interval -> Bool
overlapsBox (V3 ix iy iz) (Ray (V3 ox oy oz) (V3 dx dy dz)) (tmin, tmax) =
  isJust $ do
    (tmin', tmax') <- isect (tmin, tmax) (overlapsInterval ix ox dx)
    (tmin'', tmax'') <- isect (tmin', tmax') (overlapsInterval iy oy dy)
    isect (tmin'', tmax'') (overlapsInterval iz oz dz)

-- | A record of an intersection of a ray with a surface.
data HitRecord = HitRecord
  { hr_t :: Double -- ^ The @t@ parameter of the intersection.
  , hr_point :: Point3 -- ^ The location of the intersection.
  , hr_normal :: Vec3 -- ^ The normal vector of the surface in the direction that the ray came from. Should always be a unit vector.
  , hr_frontSide :: Bool -- ^ Whether the ray hit the "front side" (for a closed surface, the outside).
  , hr_uv :: V2 Double -- ^ The texture coordinates of the intersection.
  }