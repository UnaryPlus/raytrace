{-# LANGUAGE RecordWildCards #-}
module Graphics.Ray.Material 
  ( Material(Material)
  , lightSource, lambertian, mirror, metal, dielectric, thinPane
  ) where

import Graphics.Ray.Core
import Graphics.Ray.Geometry
import Graphics.Ray.Texture

import Linear (V3(V3), zero, normalize, dot, quadrance, (*^))
import System.Random (StdGen, random)
import Control.Monad.State (State, state)

newtype Material = Material (Ray -> HitRecord -> State StdGen (Color, Maybe (Color, Ray)))

lightSource :: Texture -> Material
lightSource (Texture tex) = Material $
  \_ (HitRecord {..}) -> pure (tex hr_point hr_uv, Nothing) 

lambertian :: Texture -> Material
lambertian (Texture tex) = Material $
  \_ (HitRecord {..}) -> do
    u <- randomUnitVector
    -- TODO: make sure hr_normal + u is not too close to 0?
    pure (zero, Just (tex hr_point hr_uv, Ray hr_point (hr_normal + u)))

mirror :: Texture -> Material
mirror (Texture tex) = Material $
  \(Ray _ dir) (HitRecord {..}) -> 
    pure (zero, Just (tex hr_point hr_uv, Ray hr_point (reflect hr_normal dir)))

metal :: Double -> Texture -> Material
metal fuzz (Texture tex) = Material $
  \(Ray _ dir) (HitRecord {..}) -> do
    u <- randomUnitVector
    let dir' = normalize (reflect hr_normal dir) + (fuzz *^ u)
    let scatter = dot dir' hr_normal > 0
    pure (zero, if scatter then Just (tex hr_point hr_uv, Ray hr_point dir') else Nothing)

-- private
refract :: Double -> Double -> Vec3 -> Vec3 -> Vec3 
refract iorRatio cosTheta normal u = let
  perp = iorRatio *^ (u + cosTheta *^ normal) 
  para = -(sqrt (abs (1 - quadrance perp)) *^ normal)
  in perp + para

dielectric :: Double -> Material
dielectric ior = Material $
  \(Ray _ dir) (HitRecord {..}) -> do
    let iorRatio = if hr_frontFace then 1/ior else ior
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

thinPane :: Texture -> Material
thinPane (Texture tex) = Material $
  \(Ray _ dir) (HitRecord {..}) ->
    pure (zero, Just (tex hr_point hr_uv, Ray hr_point dir))