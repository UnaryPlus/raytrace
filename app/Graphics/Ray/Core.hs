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

-- nearZero :: Vec3 -> Bool
-- nearZero (V3 x y z) = abs x < epsilon && abs y < epsilon && abs z < epsilon
--   where epsilon = 1e-8

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

padInterval :: Double -> Interval -> Interval
padInterval padding (tmin, tmax) = (tmin - padding, tmax + padding)

shiftInterval :: Double -> Interval -> Interval
shiftInterval x (tmin, tmax) = (tmin + x, tmax + x)

-- private
isect :: Interval -> Interval -> Maybe Interval
isect (a, b) (c, d) = let
  imin = max a c
  imax = min b d
  in if imin > imax then Nothing else Just (imin, imax)

-- private
overlapsInterval :: Interval -> Double -> Double -> Interval
overlapsInterval (tmin, tmax) x d = let
  t0 = (tmin - x) / d
  t1 = (tmax - x) / d
  in if t0 < t1 then (t0, t1) else (t1, t0)

type Box = V3 Interval

overlapsBox :: Box -> Ray -> Interval -> Bool
overlapsBox (V3 ix iy iz) (Ray (V3 ox oy oz) (V3 dx dy dz)) (tmin, tmax) =
  isJust $ do
    (tmin', tmax') <- isect (tmin, tmax) (overlapsInterval ix ox dx)
    (tmin'', tmax'') <- isect (tmin', tmax') (overlapsInterval iy oy dy)
    isect (tmin'', tmax'') (overlapsInterval iz oz dz)

fromCorners :: Point3 -> Point3 -> Box
fromCorners = liftA2 (\x y -> if x < y then (x, y) else (y, x))

allCorners :: Box -> [ Point3 ]
allCorners (V3 i1 i2 i3) = 
  [ V3 (f1 i1) (f2 i2) (f3 i3) 
  | f1 <- [ fst, snd ], f2 <- [ fst, snd ], f3 <- [ fst, snd ]
  ]

boxJoin :: Box -> Box -> Box
boxJoin = liftA2 (\(min1, max1) (min2, max2) -> (min min1 min2, max max1 max2))

boxHull :: [Box] -> Box
boxHull = foldl' boxJoin (V3 (infinity, -infinity) (infinity, -infinity) (infinity, -infinity))

padBox :: Double -> Box -> Box
padBox padding = fmap (padInterval padding)

shiftBox :: Vec3 -> Box -> Box
shiftBox = liftA2 shiftInterval

longestDim :: Box -> Dim
longestDim = argMax . fmap size

data HitRecord = HitRecord
  { hr_t :: Double
  , hr_point :: Point3
  , hr_normal :: Vec3
  , hr_frontFace :: Bool
  , hr_uv :: V2 Double
  }