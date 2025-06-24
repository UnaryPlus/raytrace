module Graphics.Ray.Core where

import Linear (V2, V3(V3), dot, quadrance, (*^), (^/))
import System.Random (RandomGen, randomR)
import Control.Monad.State (MonadState, state)
import Control.Applicative (liftA2)
import Data.Maybe (isJust)
import Data.Foldable (foldl')

infinity :: Double
infinity = 1/0

degrees :: Double -> Double
degrees x = x * pi / 180

type Vec3 = V3 Double
type Point3 = V3 Double
type Color = V3 Double

data Dim = X | Y | Z
  deriving (Eq, Ord, Enum)

component :: Dim -> V3 a -> a
component X (V3 x _ _) = x
component Y (V3 _ y _) = y
component Z (V3 _ _ z) = z

argMax :: Ord a => V3 a -> Dim
argMax (V3 x y z)
  | x > y     = if x > z then X else Z
  | otherwise = if y > z then Y else Z

-- v need not be a unit vector
reflect :: Vec3 -> Vec3 -> Vec3
reflect normal v = 
  v - 2 * dot normal v *^ normal

randomUnitVector :: (RandomGen g, MonadState g m) => m Vec3
randomUnitVector = do
  vec <- state (randomR (-1, 1))
  let q = quadrance vec
  if 1e-8 <= q && q <= 1
    then pure (vec ^/ sqrt q)
    else randomUnitVector

randomInUnitDisk :: (RandomGen g, MonadState g m) => m (V2 Double)
randomInUnitDisk = do
  vec <- state (randomR (-1, 1))
  if quadrance vec <= 1
    then pure vec
    else randomInUnitDisk

data Ray = Ray Point3 Vec3
  deriving (Show)

type Interval = (Double, Double)

size :: Interval -> Double
size (a, b) = b - a

midpoint :: Interval -> Double
midpoint (a, b) = (a + b) / 2

inInterval :: Interval -> Double -> Bool
inInterval (tmin, tmax) t = tmin < t && t < tmax

-- private
isect :: Interval -> Interval -> Maybe Interval
isect (a, b) (c, d) = let
  imin = max a c
  imax = min b d
  in if imin > imax then Nothing else Just (imin, imax)

-- private
-- TODO: edge cases
hitsInterval :: Interval -> Double -> Double -> Interval
hitsInterval (tmin, tmax) x d = let
  t0 = (tmin - x) / d
  t1 = (tmax - x) / d
  in if t0 < t1 then (t0, t1) else (t1, t0)

type Box = V3 Interval

hitsBox :: Box -> Ray -> Interval -> Bool
hitsBox (V3 ix iy iz) (Ray (V3 ox oy oz) (V3 dx dy dz)) (tmin, tmax) =
  isJust $ do
    (tmin', tmax') <- isect (tmin, tmax) (hitsInterval ix ox dx)
    (tmin'', tmax'') <- isect (tmin', tmax') (hitsInterval iy oy dy)
    isect (tmin'', tmax'') (hitsInterval iz oz dz)

fromCorners :: Point3 -> Point3 -> Box
fromCorners = liftA2 (\x y -> if x < y then (x, y) else (y, x))

boxJoin :: Box -> Box -> Box
boxJoin = liftA2 (\(min1, max1) (min2, max2) -> (min min1 min2, max max1 max2))

boxHull :: [Box] -> Box
boxHull = foldl' boxJoin (V3 (infinity, -infinity) (infinity, -infinity) (infinity, -infinity))

longestDim :: Box -> Dim
longestDim = argMax . fmap size