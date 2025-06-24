module Graphics.Ray.Geometry
  ( HitRecord(..), Geometry(Geometry)
  , sphere
  ) where

import Graphics.Ray.Core

import Linear (V2(V2), V3(V3), dot, quadrance, (*^), (^/))
import Control.Monad (guard)

data HitRecord = HitRecord
  { hr_t :: Double
  , hr_point :: Point3
  , hr_normal :: Vec3
  , hr_frontFace :: Bool
  , hr_uv :: V2 Double
  }

data Geometry = Geometry Box (Ray -> Interval -> Maybe HitRecord)

sphere :: Point3 -> Double -> Geometry
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
    Just HitRecord
      { hr_t = t
      , hr_point = point
      , hr_normal = if frontFace then outwardNormal else -outwardNormal
      , hr_frontFace = frontFace
      , hr_uv = sphereUV outwardNormal
      }
  
  in Geometry bbox hitSphere

-- With default camera settings (-z direction is forward, +y direction is up),
-- texture images will be wrapped around the sphere starting and ending on the
-- far side of the sphere.
sphereUV :: Vec3 -> V2 Double
sphereUV (V3 x y z) = V2 u v
  where
    u = atan2 x z / (2 * pi) + 0.5
    v = acos (-y) / pi 
