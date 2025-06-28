{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.Ray.Geometry where

import Graphics.Ray.Core
import Graphics.Ray.Texture
import Graphics.Ray.Material

import Linear (V2(V2), V3(V3), dot, quadrance, (*^), (^/), cross, norm, M44, inv44, (!*), V4(V4))
import qualified Linear.V4 as V4
import System.Random (StdGen, random)
import Control.Monad.State (State, state)
import Control.Monad (guard, foldM)
import Control.Applicative ((<|>))
import Data.List (sortOn)
import Data.Bifunctor (first, second)
import Data.Functor.Identity (Identity(Identity), runIdentity)
import Data.Functor ((<&>))

data Geometry m a = Geometry Box (Ray -> Interval -> m (Maybe (HitRecord, a)))

instance Functor m => Functor (Geometry m) where
  {-# SPECIALISE fmap :: (a -> b) -> Geometry Identity a -> Geometry Identity b #-}
  fmap :: (a -> b) -> Geometry m a -> Geometry m b
  fmap f (Geometry bbox hit) = Geometry bbox (\ray ival -> fmap (fmap (second f)) (hit ray ival))

pureGeometry :: Applicative m => Geometry Identity a -> Geometry m a
pureGeometry (Geometry bbox f) = Geometry bbox (\ray ival -> pure (runIdentity (f ray ival)))

boundingBox :: Geometry m a -> Box
boundingBox (Geometry b _) = b

sphere :: Point3 -> Double -> Geometry Identity ()
sphere center radius = let
  diag = V3 radius radius radius
  bbox = fromCorners (center - diag) (center + diag)

  hitSphere (Ray orig dir) bounds = Identity $ do
    let oc = center - orig
    let a = quadrance dir
    let h = dot dir oc 
    let c = quadrance oc - radius*radius

    let discriminant = h*h - a*c
    guard (discriminant >= 0)
    
    let sqrtd = sqrt discriminant
    let root1 = (h - sqrtd) / a
    let root2 = (h + sqrtd) / a

    t <- 
      if inInterval bounds root1 
        then Just root1 
      else if inInterval bounds root2
        then Just root2
      else Nothing
    
    let point = orig + t *^ dir
    let outwardNormal = (point - center) ^/ radius
    let frontFace = dot dir outwardNormal <= 0
    let hit = HitRecord
          { hr_t = t
          , hr_point = point
          , hr_normal = if frontFace then outwardNormal else -outwardNormal
          , hr_frontFace = frontFace
          , hr_uv = sphereUV outwardNormal -- only computed when necessary thanks to laziness
          }
    Just (hit, ())
  
  in Geometry bbox hitSphere

-- With default camera settings (-z direction is forward, +y direction is up),
-- texture images will be wrapped around the sphere starting and ending on the
-- far side of the sphere.
sphereUV :: Vec3 -> V2 Double
sphereUV (V3 x y z) = V2 u v
  where
    u = atan2 x z / (2 * pi) + 0.5
    v = acos (-y) / pi 

parallelogram :: Point3 -> Vec3 -> Vec3 -> Geometry Identity ()
parallelogram q u v = let
  cp = cross u v
  area = norm cp
  normal = cp ^/ area
  normalS = normal ^/ area
  n_dot_q = dot normal q

  box1 = fromCorners q (q + u + v)
  box2 = fromCorners (q + u) (q + v)
  bbox = padBox 0.0001 (boxJoin box1 box2)
  
  hitParallelogram (Ray orig dir) bounds = Identity $ do
    let denom = dot normal dir
    guard (abs denom > 1e-8)
    let t = (n_dot_q - dot normal orig) / denom
    guard (inInterval bounds t)
    let p = orig + t *^ dir
    let p_rel = p - q
    let a = normalS `dot` (p_rel `cross` v)
    let b = normalS `dot` (u `cross` p_rel)
    guard (0 <= a && a <= 1 && 0 <= b && b <= 1)
    let frontFace = denom < 0

    let hit = HitRecord
          { hr_t = t
          , hr_point = p
          , hr_normal = if frontFace then normal else -normal
          , hr_frontFace = frontFace
          , hr_uv = V2 a b
          }
    Just (hit, ())

  in Geometry bbox hitParallelogram 

cuboid :: Box -> Geometry Identity ()
cuboid (V3 (xmin, xmax) (ymin, ymax) (zmin, zmax)) = let
  dx = V3 (xmax - xmin) 0 0
  dy = V3 0 (ymax - ymin) 0
  dz = V3 0 0 (zmax - zmin)
  in group 
    [ parallelogram (V3 xmin ymin zmax) dx dy -- front
    , parallelogram (V3 xmax ymin zmin) (-dx) dy -- back
    , parallelogram (V3 xmin ymin zmin) dz dy -- left
    , parallelogram (V3 xmax ymin zmax) (-dz) dy -- right
    , parallelogram (V3 xmin ymax zmax) dx (-dz) -- top
    , parallelogram (V3 xmin ymin zmin) dx dz -- bottom
    ]

-- ASSUMES CONVEXITY
constantMedium :: Double -> Texture -> Geometry Identity () -> Geometry (State StdGen) Material
constantMedium density tex (Geometry bbox hitObj) = let
  negInvDensity = -(1 / density)
  mat = isotropic tex

  hitMedium :: Ray -> Interval -> State StdGen (Maybe (HitRecord, Material))
  hitMedium ray@(Ray orig dir) (tmin, tmax) = 
    case do (hit1, ()) <- runIdentity (hitObj ray (-infinity, infinity))
            (hit2, ()) <- runIdentity (hitObj ray (hr_t hit1, infinity))
            let t1 = max tmin (hr_t hit1)
            let t2 = min tmax (hr_t hit2)
            guard (t1 < t2)
            Just (t1, t2, hr_uv hit1) of
      Nothing -> pure Nothing -- ray is never in fog within interval
      Just (t1, t2, uv) -> state random <&> \rand ->
         do let rayScale = norm dir
            let inDist = (t2 - t1) * rayScale
            let hitDist = negInvDensity * log rand
            guard (hitDist < inDist)
            let t = t1 + hitDist / rayScale
            let hit = HitRecord
                  { hr_t = t
                  , hr_point = orig + t *^ dir
                  , hr_normal = undefined -- safe
                  , hr_frontFace = undefined -- safe
                  , hr_uv = uv
                  }
            Just (hit, mat)

  in Geometry bbox hitMedium

{-# SPECIALISE group :: [Geometry Identity a] -> Geometry Identity a #-}
group :: Monad m => [Geometry m a] -> Geometry m a
group obs = let
  bbox = boxHull (map boundingBox obs)
  
  hitGroup ray (tmin, tmax) =
    let try (tmax', knownHit) (Geometry _ hitObj) =
          hitObj ray (tmin, tmax') <&> \case
            Nothing -> (tmax', knownHit)
            Just (hit, mat) -> (hr_t hit, Just (hit, mat))
    in snd <$> foldM try (tmax, Nothing) obs
  
  in Geometry bbox hitGroup

{-# SPECIALISE bvhNode :: Geometry Identity a -> Geometry Identity a -> Geometry Identity a #-}
bvhNode :: Monad m => Geometry m a -> Geometry m a -> Geometry m a
bvhNode (Geometry bboxLeft hitLeft) (Geometry bboxRight hitRight) = let
  bbox = boxJoin bboxLeft bboxRight

  hitBvhNode ray (tmin, tmax)
    | overlapsBox bbox ray (tmin, tmax) = 
      hitLeft ray (tmin, tmax) >>= \case
        Nothing -> hitRight ray (tmin, tmax)
        res@(Just (hit, _)) -> fmap (<|> res) (hitRight ray (tmin, hr_t hit))
    | otherwise = pure Nothing
  
  in Geometry bbox hitBvhNode

data Tree a = Leaf a | Node (Tree a) (Tree a)

{-# SPECIALISE bvhTree :: Tree (Geometry Identity a) -> Geometry Identity a #-}
bvhTree :: Monad m => Tree (Geometry m a) -> Geometry m a
bvhTree = \case
  Leaf a -> a
  Node left right -> bvhNode (bvhTree left) (bvhTree right)

autoTree :: [Geometry m a] -> Tree (Geometry m a)
autoTree = \case
  [] -> error "autoTree: empty list"
  [obj] -> Leaf obj
  obs -> let
    d = longestDim (boxHull (map boundingBox obs))
    obs' = sortOn (midpoint . component d . boundingBox) obs
    (left, right) = splitAt (length obs `div` 2) obs'
    in Node (autoTree left) (autoTree right)

translate :: Vec3 -> M44 Double
translate (V3 x y z) = V4
  (V4 1 0 0 x)
  (V4 0 1 0 y)
  (V4 0 0 1 z)
  (V4 0 0 0 1)

rotateX :: Double -> M44 Double
rotateX angle = V4
  (V4 1 0 0 0)
  (V4 0 c (-s) 0)
  (V4 0 s c 0)
  (V4 0 0 0 1)
  where
    c = cos angle
    s = sin angle

rotateY :: Double -> M44 Double
rotateY angle = V4
  (V4 c 0 s 0)
  (V4 0 1 0 0)
  (V4 (-s) 0 c 0)
  (V4 0 0 0 1)
  where 
    c = cos angle
    s = sin angle

rotateZ :: Double -> M44 Double
rotateZ angle = V4
  (V4 c (-s) 0 0)
  (V4 s c 0 0)
  (V4 0 0 1 0)
  (V4 0 0 0 1)
  where
    c = cos angle
    s = sin angle

-- private
dropLast :: V4 a -> V3 a
dropLast (V4 x y z _) = V3 x y z

transform :: Functor m => M44 Double -> Geometry m a -> Geometry m a
transform m (Geometry bbox hitObj) = let
  m34 = dropLast m
  inv_m = dropLast (inv44 m)
  cornerCoords = mapM ((m34 !*) . V4.point) (allCorners bbox) :: V3 [Double]
  bbox' = fromCorners (fmap minimum cornerCoords) (fmap maximum cornerCoords)
  in Geometry bbox' $ \(Ray orig dir) ival ->
    let ray' = Ray (inv_m !* V4.point orig) (inv_m !* V4.vector dir) in
    flip (fmap . fmap . first) (hitObj ray' ival) $ \hit@(HitRecord {..}) ->
      hit { hr_point = m34 !* V4.point hr_point, hr_normal = m34 !* V4.vector hr_normal }