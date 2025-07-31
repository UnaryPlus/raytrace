{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Graphics.Ray.Material 
  ( Material(..), MaterialResult(..)
  , lightSource, pitchBlack, lambertian, lommelSeeliger, mirror, metal, dielectric, transparent, isotropic, anisotropic
  ) where

import Graphics.Ray.Core
import Graphics.Ray.Texture

import Linear (V3(V3), zero, normalize, dot, quadrance, (*^), norm)
import System.Random (StdGen, random)
import Control.Monad.State (State, state)

-- | A material is a function that takes the details of a ray-surface intersection (specifically, the direction of the
-- ray, which might not be a unit vector, along with a 'HitRecord' object) and produces an emitted color as well as a
-- 'MaterialResult' (see below).
newtype Material = Material (Vec3 -> HitRecord -> (Color, State StdGen MaterialResult))

-- | The ray tracer uses the 'MaterialResult' to compute the color from scattering, which is added to the color from
-- emission.
data MaterialResult
  = Absorb 
    -- ^ Do not create a new ray.
  | Scatter Color Vec3 
    -- ^ Create a new ray in the given direction (which need not be a unit vector)
    -- and multiply the result of recursively ray tracing the ray by the given color.
  | HemisphereF (Vec3 -> Color) 
    -- ^ Leave the task of computing a direction to the ray tracer (allowing it to send more rays toward light sources).
    -- The function is the BRDF of the material times \(\pi\). Its argument (the new ray direction) is guaranteed to be a unit
    -- vector and to have a positive dot product with 'hr_normal'.
  | SphereF (Vec3 -> Color) 
    -- ^ Similar to 'HemisphereF', but the new ray direction can be anywhere on the sphere. (Such materials are typically used
    -- with volumes like 'Graphics.Geometry.constantMedium'.) The function is the albedo times the material's phase function
    -- times \(4\pi\). Its argument is guaranteed to be a unit vector.

-- NOTE: I could generalize 'HemisphereF' and 'SphereF' by allowing the material to specify a unit vector generator (and pdf)
-- to use in the case of no redirection. 'anisotropic' and 'lommelSeeliger' could benefit from this. One issue is that it would require
-- converting a single vector into an orthonormal basis.

-- | A material that emits light and does not scatter rays.
lightSource :: Texture -> Material
lightSource (Texture tex) = Material $ \ _ HitRecord{..} -> (tex hr_point hr_uv, pure Absorb)

-- | A material that absorbs all light. This is the same as @'lightSource' ('constantTexture' 0)@, but the name reflects
-- the fact that it is not actually a light source.
pitchBlack :: Material
pitchBlack = Material $ \ _ _ -> (zero, pure Absorb)

-- | A material that exhibits Lambertian reflectance. The direction of the reflected ray is independent of the direction
-- of the incoming ray, resulting in a diffuse (non-shiny) appearance.
lambertian :: Texture -> Material
lambertian (Texture tex) = Material $
  \_ (HitRecord {..}) -> (zero, pure $ HemisphereF $ const (tex hr_point hr_uv))

-- | A material that exhibits Lommel-Seeliger reflectance.
lommelSeeliger :: Texture -> Material
lommelSeeliger (Texture tex) = Material $
  \inDir (HitRecord {..}) -> (zero,) $ pure $ HemisphereF $ \outDir -> let
    mu0 = -(dot inDir hr_normal / norm inDir)
    mu1 = dot outDir hr_normal
    in 0.25 / (mu0 + mu1) *^ tex hr_point hr_uv 

-- | A colored mirror. (For no color, use @'constantTexture' 1@.)
mirror :: Texture -> Material
mirror (Texture tex) = Material $
  \dir (HitRecord {..}) -> 
    (zero, pure $ Scatter (tex hr_point hr_uv) (reflect hr_normal dir))

-- | A metallic-looking material that reflects rays inexactly. The larger the first argument is, the less shiny
-- the material. @'metal' 0@ behaves the same as 'mirror'.
metal :: Double -> Texture -> Material
metal fuzz (Texture tex) = Material $
  \dir (HitRecord {..}) -> (zero,) $ do
    u <- randomUnitVector
    let dir' = normalize (reflect hr_normal dir) + (fuzz *^ u)
    let scatter = dot dir' hr_normal > 0
    pure (if scatter then Scatter (tex hr_point hr_uv) dir' else Absorb)

-- [private]
refract :: Double -> Double -> Vec3 -> Vec3 -> Vec3 
refract iorRatio cosTheta normal u = let
  perp = iorRatio *^ (u + cosTheta *^ normal) 
  para = -(sqrt (abs (1 - quadrance perp)) *^ normal)
  in perp + para

-- | A material that either reflects or refracts all incoming rays, like clear glass.
-- The argument is the index of refraction relative to the surrounding medium.
dielectric :: Double -> Material
dielectric ior = Material $
  \dir (HitRecord {..}) -> (zero,) $ do
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
    
    pure (Scatter (V3 1 1 1) dir')

-- | A material that lets light through, with the given tint.
transparent :: Texture -> Material
transparent (Texture tex) = Material $
  \dir (HitRecord {..}) ->
    (zero, pure $ Scatter (tex hr_point hr_uv) dir)

-- | A material that scatters an incoming ray in a direction chosen uniformly at random from the unit sphere. 
-- (Typically used with 'Graphics.Geometry.constantMedium'.)
isotropic :: Texture -> Material
isotropic (Texture tex) = Material $
  \_ (HitRecord {..}) -> (zero, pure $ SphereF $ const (tex hr_point hr_uv))

-- | A material that scatters an incoming ray according to the Henyey-Greenstein distribution. The first parameter
-- should be in the range (-1, 1); negative values result in more backward scattering and positive values result in
-- more forward scattering.
-- (Typically used with 'Graphics.Geometry.constantMedium'.)
anisotropic :: Double -> Texture -> Material
anisotropic g (Texture tex) = Material $
  \inDir (HitRecord {..}) -> (zero,) $ pure $ SphereF $ \outDir -> let
    mu = dot inDir outDir / norm inDir
    hg = (1 - g*g) / (1 + g*g - 2*g*mu)**1.5 
    in hg *^ tex hr_point hr_uv
