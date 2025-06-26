{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module Graphics.Ray.Geometry where

import Graphics.Ray.Core

import Linear (V2(V2), V3(V3), dot, quadrance, (*^), (^/))
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
  

