{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.Ray 
  ( -- * Camera
    CameraSettings(..), defaultCameraSettings
    -- * Ray Tracing
  , ToRandom(toRandom), raytrace
    -- * Image IO
  , readImage, writeImage, writeImageSqrt
    -- * Re-exports
  , module Graphics.Ray.Core
  , module Graphics.Ray.Geometry
  , module Graphics.Ray.Material
  , module Graphics.Ray.Texture
  , module Graphics.Ray.Noise
  ) where

import Graphics.Ray.Core
import Graphics.Ray.Geometry
import Graphics.Ray.Material
import Graphics.Ray.Texture
import Graphics.Ray.Noise

import Linear (V2(V2), V3(V3), (*^), (^*), normalize, cross, (^/), zero)
import System.Random (StdGen, random, splitGen)
import Data.Massiv.Array (B, D, S, U, Ix2((:.)), (!))
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.IO as I
import Graphics.Pixel.ColorSpace (SRGB, Linearity(Linear, NonLinear))
import qualified Graphics.Pixel.ColorSpace as C
import Control.Monad.State (State, state, evalState)
import Control.Monad (replicateM)
import Data.Functor.Identity (Identity, runIdentity)

data CameraSettings = CameraSettings
  { cs_center :: Point3 -- ^ Camera position 
  , cs_lookAt :: Point3 -- ^ Point for the camera to look at 
  , cs_up :: Vec3 -- ^ Camera \"up\" vector 
  , cs_vfov :: Double -- ^ Vertical field of view (in radians) 
  , cs_aspectRatio :: Double -- ^ Width-to-height ratio of image 
  , cs_imageWidth :: Int -- ^ Image width in pixels 
  , cs_samplesPerPixel :: Int -- ^ Number of top-level rays created per pixel 
  , cs_maxRecursionDepth :: Int -- ^ Number of times a ray can reflect before recursion stops 
  , cs_background :: Ray -> Color -- ^ Background color (which can depend on direction) 
  , cs_defocusAngle :: Double -- ^ If this is positive, the image will be somewhat blurry in the foreground and background, 
                              -- with only a single plane in focus (like an image produced by a real camera)
  , cs_focusDist :: Double -- ^ Distance from the camera to the plane of focus (only matters if defocus angle is nonzero) 
  }

-- | By default, the camera is positioned at the origin looking in the negative z direction, with the positive y direction being upward
-- (and the positive x direction being rightward). The remaining attributes are as follows:
-- @
--   cs_vfov = pi / 2
--   cs_aspectRatio = 1.0
--   cs_imageWidth = 100
--   cs_samplesPerPixel = 10
--   cs_maxRecursionDepth = 10
--   cs_background = const (V3 1 1 1)
--   cs_defocusAngle = 0.0
--   cs_focusDist = 10.0
-- @
defaultCameraSettings :: CameraSettings
defaultCameraSettings = CameraSettings
  { cs_center = V3 0 0 0
  , cs_lookAt = V3 0 0 (-1)
  , cs_up = V3 0 1 0
  , cs_vfov = pi / 2
  , cs_aspectRatio = 1.0
  , cs_imageWidth = 100
  , cs_samplesPerPixel = 10
  , cs_maxRecursionDepth = 10
  , cs_background = const (V3 1 1 1)
  , cs_defocusAngle = 0.0
  , cs_focusDist = 10.0
  }

class ToRandom m where
  toRandom :: m a -> State StdGen a

instance ToRandom Identity where
  toRandom :: Identity a -> State StdGen a
  toRandom = pure . runIdentity

instance ToRandom (State StdGen) where
  toRandom :: State StdGen a -> State StdGen a
  toRandom = id

-- | Produce an image from the given camera settings, world, and seed.
raytrace :: ToRandom m => CameraSettings -> Geometry m Material -> StdGen -> A.Matrix D Color
raytrace (CameraSettings {..}) (Geometry _ hitWorld) seed = let
  imageHeight = round (fromIntegral cs_imageWidth / cs_aspectRatio)
  viewportHeight = cs_focusDist * tan (cs_vfov / 2) * 2
  viewportWidth = viewportHeight * fromIntegral cs_imageWidth / fromIntegral imageHeight

  w = normalize (cs_center - cs_lookAt)
  u = normalize (cross cs_up w) 
  v = cross w u

  across = viewportWidth *^ u
  down = -(viewportHeight *^ v)

  topLeft = cs_center - w ^* cs_focusDist - across ^/ 2 - down ^/ 2
  pixelU = across ^/ fromIntegral cs_imageWidth
  pixelV = down ^/ fromIntegral imageHeight

  defocusRadius = cs_focusDist * tan (cs_defocusAngle / 2)
  defocusDiskU = u ^* defocusRadius
  defocusDiskV = v ^* defocusRadius
  
  sampleDefocusDisk :: State StdGen Point3
  sampleDefocusDisk = do
    V2 x y <- randomInUnitDisk
    pure (cs_center + x *^ defocusDiskU + y *^ defocusDiskV)

  samplePixel :: Int -> Int -> State StdGen Point3
  samplePixel i j = do
    x <- state random
    y <- state random
    pure (topLeft + (fromIntegral i + x) *^ pixelU + (fromIntegral j + y) *^ pixelV)

  getRay :: Int -> Int -> State StdGen Ray
  getRay i j = do
    origin <- sampleDefocusDisk
    target <- samplePixel i j
    pure (Ray origin (target - origin))

  rayColor :: Int -> Ray -> State StdGen Color
  rayColor depth ray
    | depth <= 0 = pure zero
    | otherwise =
    toRandom (hitWorld ray (0.0001, infinity)) >>= \case
      Nothing -> pure (cs_background ray)
      Just (hit, Material mat) -> 
        mat ray hit >>= \case
          (emitted, Nothing) -> pure emitted
          (emitted, Just (attenuation, ray')) -> do
            c <- rayColor (depth - 1) ray'
            pure (emitted + attenuation * c)
  
  pixelColor :: Int -> Int -> State StdGen Color
  pixelColor i j = do
    colors <- replicateM cs_samplesPerPixel (getRay i j >>= rayColor cs_maxRecursionDepth)
    pure (sum colors ^/ fromIntegral cs_samplesPerPixel)

  -- array of random seeds for each pixel (constructed using splitGen)
  seeds :: A.Matrix B StdGen
  (_, seeds) = A.randomArrayS seed (A.Sz (imageHeight :. cs_imageWidth)) splitGen

  in A.makeArray A.Par (A.Sz (imageHeight :. cs_imageWidth)) (\ix@(j :. i) -> evalState (pixelColor i j) (seeds ! ix))

-- | Read an image file, converting each pixel to linear RGB color space.
readImage :: FilePath -> IO (A.Matrix U Color)
readImage path = A.compute . A.map fromPixel <$> (I.readImageAuto path :: IO (A.Matrix S (C.Pixel (SRGB 'Linear) Double)))
  where
    fromPixel :: C.Pixel (SRGB 'Linear) Double -> Color
    fromPixel (C.Pixel (C.ColorSRGB r g b)) = V3 r g b

-- | Write an array of linear RGB colors to an image file.
writeImage :: (A.Source r Color) => FilePath -> A.Matrix r Color -> IO ()
writeImage path m = I.writeImageAuto path (A.map toPixel m)
  where
    toPixel :: Color -> C.Pixel (SRGB 'Linear) Double
    toPixel (V3 r g b) = C.Pixel (C.ColorSRGB r g b)

-- | Write an array to an image file, using a slightly incorrect color space conversion function.
-- This function exists so that TODO
writeImageSqrt :: (A.Source r Color) => FilePath -> A.Matrix r Color -> IO ()
writeImageSqrt path m = I.writeImageAuto path (A.map toPixel m)
  where
    toPixel :: Color -> C.Pixel (SRGB 'NonLinear) Double
    toPixel (V3 r g b) = C.Pixel (C.ColorSRGB (sqrt r) (sqrt g) (sqrt b))