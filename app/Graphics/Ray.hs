{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Graphics.Ray where

import Graphics.Ray.Core
import Graphics.Ray.Material
import Graphics.Ray.SceneObject

import Linear (V2(V2), V3(V3), (*^), (^*), normalize, cross, (^/), zero)
import System.Random (StdGen, random, splitGen)
import Data.Massiv.Array (B, D, S, Ix2((:.)), (!))
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.IO as I
import Graphics.Pixel.ColorSpace (SRGB, Linearity(Linear, NonLinear))
import qualified Graphics.Pixel.ColorSpace as C
import Control.Monad.State (State, state, evalState)
import Control.Monad (replicateM)

data CameraSettings = CameraSettings
  { cs_center :: Point3
  , cs_lookAt :: Point3
  , cs_up :: Vec3
  , cs_vfov :: Double
  , cs_aspectRatio :: Double
  , cs_imageWidth :: Int
  , cs_samplesPerPixel :: Int
  , cs_maxRecursionDepth :: Int
  , cs_background :: Ray -> Color
  , cs_defocusAngle :: Double
  , cs_focusDist :: Double
  }

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
  , cs_background = const (V3 0 0 0)
  , cs_defocusAngle = 0.0
  , cs_focusDist = 10.0
  }

-- TODO: modify to return seed
raytrace :: CameraSettings -> SceneObject -> StdGen -> A.Matrix D Color
raytrace (CameraSettings {..}) (SceneObject _ hitWorld) seed = let
  imageHeight = ceiling (fromIntegral cs_imageWidth / cs_aspectRatio)
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
    case hitWorld ray (0.0001, infinity) of
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
  seeds = snd (A.randomArrayS seed (A.Sz (imageHeight :. cs_imageWidth)) splitGen) :: A.Matrix B StdGen

  in A.makeArray A.Par (A.Sz (imageHeight :. cs_imageWidth)) (\ix@(j :. i) -> evalState (pixelColor i j) (seeds ! ix))

readImage :: (A.Manifest r Color) => FilePath -> IO (A.Matrix r Color)
readImage path = A.compute . A.map fromPixel <$> (I.readImageAuto path :: IO (A.Matrix S (C.Pixel (SRGB 'Linear) Double)))
  where
    fromPixel :: C.Pixel (SRGB 'Linear) Double -> Color
    fromPixel (C.Pixel (C.ColorSRGB r g b)) = V3 r g b

writeImage :: (A.Source r Color) => FilePath -> A.Matrix r Color -> IO ()
writeImage path m = I.writeImageAuto path (A.map toPixel m)
  where
    toPixel :: Color -> C.Pixel (SRGB 'Linear) Double
    toPixel (V3 r g b) = C.Pixel (C.ColorSRGB r g b)

-- Write image using incorrect color space conversion from "Ray Tracing in One Weekend"
writeImageRTW :: (A.Source r Color) => FilePath -> A.Matrix r Color -> IO ()
writeImageRTW path m = I.writeImageAuto path (A.map toPixel m)
  where
    toPixel :: Color -> C.Pixel (SRGB 'NonLinear) Double
    toPixel (V3 r g b) = C.Pixel (C.ColorSRGB (sqrt r) (sqrt g) (sqrt b))