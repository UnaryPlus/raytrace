{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.Ray.Geometry where

import Graphics.Ray.Core

import Linear (V2(V2), V3(V3), dot, quadrance, (*^), (^/), cross, norm, M44, inv44, (!*), V4 (V4))
import qualified Linear.V4 as V4
import Control.Monad (guard)
import Control.Applicative ((<|>))
import Data.List (sortOn)

data Geometry a = Geometry Box (Ray -> Interval -> Maybe (HitRecord, a))

instance Functor Geometry where
  fmap :: (a -> b) -> Geometry a -> Geometry b
  fmap f (Geometry bbox hit) = Geometry bbox (fmap (fmap (fmap (fmap f))) hit) -- lol
  -- TODO: is this implementation inefficient?

boundingBox :: Geometry a -> Box
boundingBox (Geometry b _) = b

sphere :: Point3 -> Double -> Geometry ()
sphere center radius = let
  diag = V3 radius radius radius
  bbox = fromCorners (center - diag) (center + diag)

  hitSphere (Ray orig dir) bounds = do
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
          , hr_uv = sphereUV outwardNormal
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

parallelogram :: Point3 -> Vec3 -> Vec3 -> Geometry ()
parallelogram q u v = let
  cp = cross u v
  area = norm cp
  normal = cp ^/ area
  normalS = normal ^/ area
  n_dot_q = dot normal q

  box1 = fromCorners q (q + u + v)
  box2 = fromCorners (q + u) (q + v)
  bbox = padBox 0.0001 (boxJoin box1 box2) -- TODO: move out into constant
  
  hitParallelogram (Ray orig dir) bounds = do
    let denom = dot normal dir
    guard (abs denom > 1e-8) -- TODO: move out into constant
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

cuboid :: Box -> Geometry ()
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

group :: [Geometry a] -> Geometry a
group obs = let
  bbox = boxHull (map boundingBox obs)
  
  hitGroup ray (tmin, tmax) =
    let try (tmax', knownHit) (Geometry _ hitObj) =
          case hitObj ray (tmin, tmax') of
            Nothing -> (tmax', knownHit)
            Just (hit, mat) -> (hr_t hit, Just (hit, mat))
    in snd (foldl try (tmax, Nothing) obs)
  
  in Geometry bbox hitGroup

bvhNode :: Geometry a -> Geometry a -> Geometry a
bvhNode (Geometry bboxLeft hitLeft) (Geometry bboxRight hitRight) = let
  bbox = boxJoin bboxLeft bboxRight

  hitBvhNode ray (tmin, tmax)
    | hitsBox bbox ray (tmin, tmax) = 
      case hitLeft ray (tmin, tmax) of
        Nothing -> hitRight ray (tmin, tmax)
        res@(Just (hit, _)) -> hitRight ray (tmin, hr_t hit) <|> res
    | otherwise = Nothing
  
  in Geometry bbox hitBvhNode

data Tree a = Leaf a | Node (Tree a) (Tree a)

bvhTree :: Tree (Geometry a) -> Geometry a
bvhTree = \case
  Leaf a -> a
  Node left right -> bvhNode (bvhTree left) (bvhTree right)

autoTree :: [Geometry a] -> Tree (Geometry a)
autoTree = \case
  [] -> error "autoTree: empty list"
  [obj] -> Leaf obj
  obs -> let
    d = longestDim (boxHull (map boundingBox obs))
    obs' = sortOn (midpoint . component d . boundingBox) obs
    (left, right) = splitAt (length obs `div` 2) obs'
    in Node (autoTree left) (autoTree right)
  
translate :: Vec3 -> Geometry a -> Geometry a
translate v (Geometry bbox hitObj) =
  Geometry (shiftBox v bbox) $ \(Ray orig dir) ival -> do
    let ray' = Ray (orig - v) dir
    (hit, mat) <- hitObj ray' ival
    Just (hit { hr_point = hr_point hit + v }, mat)

-- TODO: more efficient definitions?
rotateX :: Double -> Geometry a -> Geometry a
rotateX angle = 
  let c = cos angle; s = sin angle in
  affineTransform $ V4
    (V4 1 0 0 0)
    (V4 0 c (-s) 0)
    (V4 0 s c 0)
    (V4 0 0 0 1)

rotateY :: Double -> Geometry a -> Geometry a
rotateY angle =
  let c = cos angle; s = sin angle in
  affineTransform $ V4
    (V4 c 0 s 0)
    (V4 0 1 0 0)
    (V4 (-s) 0 c 0)
    (V4 0 0 0 1)

rotateZ :: Double -> Geometry a -> Geometry a
rotateZ angle = 
  let c = cos angle; s = sin angle in
  affineTransform $ V4
    (V4 c (-s) 0 0)
    (V4 s c 0 0)
    (V4 0 0 1 0)
    (V4 0 0 0 1)

-- private
dropLast :: V4 a -> V3 a
dropLast (V4 x y z _) = V3 x y z

affineTransform :: M44 Double -> Geometry a -> Geometry a
affineTransform m (Geometry bbox hitObj) = let
  m34 = dropLast m
  inv_m = dropLast (inv44 m)
  bbox' = undefined -- TODO
  in Geometry bbox' $ \(Ray orig dir) ival -> do
    let ray' = Ray (inv_m !* V4.point orig) (inv_m !* V4.vector dir)
    (hit@HitRecord{..}, mat) <- hitObj ray' ival
    Just (hit { hr_point = m34 !* V4.point hr_point, hr_normal = m34 !* V4.vector hr_normal }, mat)