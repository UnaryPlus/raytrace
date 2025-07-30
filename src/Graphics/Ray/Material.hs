{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Graphics.Ray.Material 
  ( Material(..), MaterialResult(..)
  , lightSource, pitchBlack, lambertian, mirror, metal, dielectric, transparent, isotropic, anisotropic
  ) where

import Graphics.Ray.Core
import Graphics.Ray.Texture

import Linear (V3(V3), zero, normalize, dot, quadrance, (*^), norm)
import System.Random (StdGen, random)
import Control.Monad.State (State, state)

-- | A material is a function that, given the details of a ray-surface intersection, produces an emitted color
-- (which is @0@ for all of the materials in this module save 'lightSource') and potentially a reflected ray
-- along with a color to scale the result of tracing said ray by.
newtype Material = Material (Vec3 -> HitRecord -> (Color, State StdGen MaterialResult))

data MaterialResult
  = Absorb
  | Scatter Color Vec3 -- TODO: replace Ray with Vec3?
  | HemisphereF (Vec3 -> Color) -- BRDF * pi (argument is normalized and has positive dot product with hr_normal)
  | SphereF (Vec3 -> Color) -- Albedo * phase function * 4 pi (argument is normalized)

-- NOTE: I could generalize 'HemisphereF' and 'SphereF' by allowing the material to specify a unit vector generator (and pdf)
-- to use in the case of no redirection. 'anisotropic', for example, could benefit from this. One issue is that it would require
-- converting a single vector into an orthonormal basis.

-- | A material that emits light and does not reflect rays.
lightSource :: Texture -> Material
lightSource (Texture tex) = Material $ \ _ HitRecord{..} -> (tex hr_point hr_uv, pure Absorb)

-- | A material that absorbs all light. Identical in principle to @'lambertian' ('constantTexture' 0)@, @'isotropic' ('constantTexture' 0)@,
-- and so on, but avoids unecessary computation.
pitchBlack :: Material
pitchBlack = Material $ \ _ _ -> (zero, pure Absorb)

-- | A material that exhibits Lambertian reflectance. The direction of the reflected ray is independent of the direction
-- of the incoming ray, resulting in a diffuse (non-shiny) appearance.
lambertian :: Texture -> Material
lambertian (Texture tex) = Material $
  \_ (HitRecord {..}) -> (zero, pure $ HemisphereF $ const (tex hr_point hr_uv))

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

-- | A material that lets all light through, with the given tint.
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
