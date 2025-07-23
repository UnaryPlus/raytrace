{-# LANGUAGE RecordWildCards #-}
module Graphics.Ray.Material 
  ( Material(..), MaterialReturn(..)
  , lightSource, pitchBlack, lambertian, mirror, metal, dielectric, transparent, isotropic
  ) where

import Graphics.Ray.Core
import Graphics.Ray.Texture

import Linear (V3(V3), zero, normalize, dot, quadrance, (*^), (^*), (^/))
import System.Random (StdGen, random)
import Control.Monad.State (State, state)

-- | A material is a function that, given the details of a ray-surface intersection, produces an emitted color
-- (which is @0@ for all of the materials in this module save 'lightSource') and potentially a reflected ray
-- along with a color to scale the result of tracing said ray by.
data Material 
  = OldType (Ray -> HitRecord -> State StdGen (Color, Maybe (Color, Ray))) -- TODO: rename
  | NewType (Ray -> HitRecord -> MaterialReturn) -- TODO: rename

data MaterialReturn = MaterialReturn -- TODO: rename
  { mr_multiplier :: Vec3 -> Color -- 'attenuation' (prior to dividing by combined pdf)
  , mr_generate :: State StdGen Vec3 -- distribution for non-redirected rays (SHOULD BE NORMALIZED)
  , mr_pdf :: Vec3 -> Double -- pdf for mr_generate
  }

-- | A material that emits light and does not reflect rays.
lightSource :: Texture -> Material
lightSource (Texture tex) = OldType $
  \_ (HitRecord {..}) -> pure (tex hr_point hr_uv, Nothing)

-- | A material that absorbs all light. Identical in principle to @'lambertian' ('constantTexture' 0)@, @'isotropic' ('constantTexture' 0)@,
-- and so on, but avoids unecessary computation.
pitchBlack :: Material
pitchBlack = OldType $ \ _ _ -> pure (zero, Nothing)

-- | A material that exhibits Lambertian reflectance. The direction of the reflected ray is independent of the direction
-- of the incoming ray, resulting in a diffuse (non-shiny) appearance.
lambertian :: Texture -> Material
lambertian (Texture tex) = NewType $
  \_ (HitRecord {..}) -> MaterialReturn
    { mr_multiplier = \dir -> tex hr_point hr_uv ^* (max 0 (dot dir hr_normal) / pi)
    , mr_generate = (\u -> normalize (hr_normal + u)) <$> randomUnitVector
    , mr_pdf = \dir -> max 0 (dot dir hr_normal) / pi
    }

-- | A colored mirror. (For no color, use @'constantTexture' 1@.)
mirror :: Texture -> Material
mirror (Texture tex) = OldType $
  \(Ray _ dir) (HitRecord {..}) -> 
    pure (zero, Just (tex hr_point hr_uv, Ray hr_point (reflect hr_normal dir)))

-- | A metallic-looking material that reflects rays inexactly. The larger the first argument is, the less shiny
-- the material. @'metal' 0@ behaves the same as 'mirror'.
metal :: Double -> Texture -> Material
metal fuzz (Texture tex) = OldType $
  \(Ray _ dir) (HitRecord {..}) -> do
    u <- randomUnitVector
    let dir' = normalize (reflect hr_normal dir) + (fuzz *^ u)
    let scatter = dot dir' hr_normal > 0
    pure (zero, if scatter then Just (tex hr_point hr_uv, Ray hr_point dir') else Nothing)

-- [private]
refract :: Double -> Double -> Vec3 -> Vec3 -> Vec3 
refract iorRatio cosTheta normal u = let
  perp = iorRatio *^ (u + cosTheta *^ normal) 
  para = -(sqrt (abs (1 - quadrance perp)) *^ normal)
  in perp + para

-- | A material that either reflects or refracts all incoming rays, like clear glass.
-- The argument is the index of refraction relative to the surrounding medium.
dielectric :: Double -> Material
dielectric ior = OldType $
  \(Ray _ dir) (HitRecord {..}) -> do
    let iorRatio = if hr_frontSide then 1/ior else ior
    let u = normalize dir
    let cosTheta = min 1 (dot hr_normal (-u))
    let sinTheta = sqrt (1 - cosTheta * cosTheta)
    let cannotRefract = iorRatio * sinTheta > 1

    let r0 = (1 - iorRatio) / (1 + iorRatio)
    let r0' = r0 * r0
    let reflectance = r0' + (1 - r0') * (1 - cosTheta)**5 -- Schlick approximation
    x <- state random

    let dir' = if cannotRefract || x < reflectance
          then reflect hr_normal u
          else refract iorRatio cosTheta hr_normal u
    
    pure (zero, Just (V3 1 1 1, Ray hr_point dir'))    

-- | A material that lets all light through, with the given tint.
transparent :: Texture -> Material
transparent (Texture tex) = OldType $
  \(Ray _ dir) (HitRecord {..}) ->
    pure (zero, Just (tex hr_point hr_uv, Ray hr_point dir))

-- | A material that scatters an incoming ray in a direction chosen uniformly at random from the unit sphere. 
-- (Typically used with 'Graphics.Geometry.constantMedium'.)
isotropic :: Texture -> Material
isotropic (Texture tex) = NewType $
  \_ (HitRecord {..}) -> MaterialReturn
    { mr_multiplier = const (tex hr_point hr_uv * 0.25 / pi)
    , mr_generate = randomUnitVector
    , mr_pdf = const (0.25 / pi)
    }
    -- OldType $ 
    -- \_ (HitRecord {..}) -> do
    --   dir <- randomUnitVector
    --   pure (zero, Just (tex hr_point hr_uv, Ray hr_point dir))
