{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Graphics.Ray.Core
import Graphics.Ray.Material
import Graphics.Ray.Texture
import Graphics.Ray.Geometry
import Graphics.Ray

import Linear (V3(V3), (*^), normalize, norm, (!*!))
import System.Random (StdGen, newStdGen, randomR, random)
import Control.Monad.State (State, runState, state)
import Control.Monad (forM)
import Control.Applicative (liftA2)
import Data.Massiv.Array (Matrix, U)

sky :: Ray -> Color
sky (Ray _ (normalize -> V3 _ y _)) = 
  let a = 0.5 * (y + 1) in
  (1 - a) *^ V3 1 1 1 + a *^ V3 0.5 0.7 1

metalTest :: IO ()
metalTest = let
  materialGround = lambertian (constantTexture (V3 0.8 0.8 0.0))
  materialCenter = lambertian (constantTexture (V3 0.1 0.2 0.5))
  materialLeft = dielectric 1.5
  materialBubble = dielectric (1 / 1.5)
  materialRight = metal 1.0 (constantTexture (V3 0.8 0.6 0.2))

  world = group 
    [ materialGround <$ sphere (V3 0 (-100.5) (-1)) 100
    , materialCenter <$ sphere (V3 0 0 (-1.2)) 0.5
    , materialLeft <$ sphere (V3 (-1) 0 (-1)) 0.5 
    , materialBubble <$ sphere (V3 (-1) 0 (-1)) 0.4
    , materialRight <$ sphere (V3 1 0 (-1)) 0.5
    ]

  settings = defaultCameraSettings 
    { cs_aspectRatio = 16/9
    , cs_imageWidth = 400
    , cs_samplesPerPixel = 100
    , cs_maxRecursionDepth = 50
    , cs_background = sky
    , cs_center = V3 (-2) 2 1
    , cs_lookAt = V3 0 0 (-1)
    , cs_vfov = degrees 20
    , cs_defocusAngle = degrees 10
    , cs_focusDist = 3.4
    }

  in do
  seed <- newStdGen
  writeImageRTW "test_image.png" $ raytrace settings world seed

checkerTest :: IO ()
checkerTest = let
  checker = lambertian (checkerTexture 0.32 (V3 0.2 0.3 0.1) (V3 0.9 0.9 0.9))

  world = group
    [ checker <$ sphere (V3 0 (-10) 0) 10
    , checker <$ sphere (V3 0 10 0) 10
    ]
  
  settings = defaultCameraSettings
    { cs_aspectRatio = 16 / 9
    , cs_imageWidth = 400
    , cs_samplesPerPixel = 100
    , cs_maxRecursionDepth = 50
    , cs_background = sky
    , cs_vfov = degrees 20
    , cs_center = V3 13 2 3
    , cs_lookAt = V3 0 0 0
    }

  in do
  seed <- newStdGen
  writeImageRTW "test_image.png" $ raytrace settings world seed

noiseTest :: IO ()
noiseTest = let
  groundMaterial = lambertian (noiseTexture 4)
  ballMaterial = lambertian (marbleTexture 4)

  world = group 
    [ groundMaterial <$ sphere (V3 0 (-1000) 0) 1000
    , ballMaterial <$ sphere (V3 0 2 0) 2
    ]
  
  settings = defaultCameraSettings
    { cs_aspectRatio = 16 / 9
    , cs_imageWidth = 400
    , cs_samplesPerPixel = 100
    , cs_maxRecursionDepth = 50
    , cs_background = sky
    , cs_vfov = degrees 20
    , cs_center = V3 13 2 3
    , cs_lookAt = V3 0 0 0
    }
    
  in do
  seed <- newStdGen
  writeImageRTW "test_image.png" $ raytrace settings world seed

quadTest :: IO ()
quadTest = let
  red = lambertian (constantTexture (V3 1.0 0.2 0.2))
  green = lambertian (constantTexture (V3 0.2 1.0 0.2))
  blue = lambertian (constantTexture (V3 0.2 0.2 1.0))
  orange = lambertian (constantTexture (V3 1.0 0.5 0.0))
  teal = lambertian (constantTexture (V3 0.2 0.8 0.8))

  world = group
    [ red <$ parallelogram (V3 (-3) (-2) 5) (V3 0 0 (-4)) (V3 0 4 0) 
    , green <$ parallelogram (V3 (-2) (-2) 0) (V3 4 0 0) (V3 0 4 0)
    , blue <$ parallelogram (V3 3 (-2) 1) (V3 0 0 4) (V3 0 4 0)
    , orange <$ parallelogram (V3 (-2) 3 1) (V3 4 0 0) (V3 0 0 4)
    , teal <$ parallelogram (V3 (-2) (-3) 5) (V3 4 0 0) (V3 0 0 (-4))
    ]
  
  settings = defaultCameraSettings
    { cs_aspectRatio = 1
    , cs_imageWidth = 400
    , cs_samplesPerPixel = 100
    , cs_maxRecursionDepth = 50
    , cs_background = sky
    , cs_vfov = degrees 80
    , cs_center = V3 0 0 9
    , cs_lookAt = V3 0 0 0
    }

  in do
  seed <- newStdGen
  writeImageRTW "test_image.png" $ raytrace settings world seed

cuboidTest :: IO ()
cuboidTest = do
  globe <- readImage "images/earthmap.jpg"
  let globeMaterial = lambertian (imageTexture (globe :: Matrix U Color))
  let object = globeMaterial <$ cuboid (fromCorners (-V3 1 2 0.5) (V3 1 2 0.5))
  let world = transform (translate (V3 0 0 (-3)) !*! rotateX (degrees 60)) object
  let settings = defaultCameraSettings { cs_imageWidth = 300 }
  writeImage "test_image.png" . raytrace settings world =<< newStdGen

demo1 :: IO ()
demo1 = let
  materialGround = lambertian (constantTexture (V3 0.5 0.5 0.5))
  materialGlass = dielectric 1.5
  materialDiffuse = lambertian (constantTexture (V3 0.4 0.2 0.1))
  materialMirror = mirror (constantTexture (V3 0.7 0.6 0.5))

  bigSpheres =
    [ materialGround <$ sphere (V3 0 (-1000) 0) 1000
    , materialGlass <$ sphere (V3 0 1 0) 1
    , materialDiffuse <$ sphere (V3 (-4) 1 0) 1
    , materialMirror <$ sphere (V3 4 1 0) 1
    ]

  genWorld :: State StdGen (Geometry Material)
  genWorld = do
    fmap (bvhTree . autoTree . (bigSpheres ++) . concat) $ forM (liftA2 (,) [-11..10] [-11..10]) $ \(a, b) -> do
      offsetX <- state (randomR (0, 0.9))
      offsetZ <- state (randomR (0, 0.9))
      let center = V3 (a + offsetX) 0.2 (b + offsetZ)

      if norm (center - V3 4 0.2 0) <= 0.9 then pure [] else do
        chooseMat <- state random
        mat <- 
          if (chooseMat :: Double) < 0.8 then do
            color <- liftA2 (*) (state random) (state random)
            pure (lambertian (constantTexture color))
          else if chooseMat < 0.95 then do
            fuzz <- state (randomR (0, 0.5))
            color <- state (randomR (0.5, 1))
            pure (metal fuzz (constantTexture color))
          else pure materialGlass
        pure [ mat <$ sphere center 0.2 ]
  
  settings = defaultCameraSettings
    { cs_aspectRatio = 16 / 9
    , cs_imageWidth = 1200
    , cs_samplesPerPixel = 500
    , cs_maxRecursionDepth = 50
    , cs_vfov = degrees 20
    , cs_center = V3 13 2 3
    , cs_lookAt = V3 0 0 0
    , cs_defocusAngle = degrees 0.6
    , cs_focusDist = 10
    , cs_background = sky
    }

  in do
  seed <- newStdGen
  let (world, seed') = runState genWorld seed
  writeImageRTW "test_image.png" $ raytrace settings world seed'

cornellBox :: IO ()
cornellBox = let
  red = lambertian (constantTexture (V3 0.65 0.05 0.05))
  white = lambertian (constantTexture (V3 0.73 0.73 0.73))
  green = lambertian (constantTexture (V3 0.12 0.45 0.15))
  light = lightSource (constantTexture (V3 15 15 15))

  world = group
    [ green <$ parallelogram (V3 555 0 0) (V3 0 555 0) (V3 0 0 555)
    , red <$ parallelogram (V3 0 0 0) (V3 0 555 0) (V3 0 0 555)
    , light <$ parallelogram (V3 343 554 332) (V3 (-130) 0 0) (V3 0 0 (-105))
    , white <$ parallelogram (V3 0 0 0) (V3 555 0 0) (V3 0 0 555)
    , white <$ parallelogram (V3 555 555 555) (V3 (-555) 0 0) (V3 0 0 (-555))
    , white <$ parallelogram (V3 0 0 555) (V3 555 0 0) (V3 0 555 0)
    , transform (translate (V3 265 0 295) !*! rotateY (degrees 15)) $ white <$ cuboid (fromCorners (V3 0 0 0) (V3 165 330 165))
    , transform (translate (V3 130 0 65) !*! rotateY (degrees (-18))) $ white <$ cuboid (fromCorners (V3 0 0 0) (V3 165 165 165))
    ]

  settings = defaultCameraSettings
    { cs_aspectRatio = 1.0
    , cs_imageWidth = 600
    , cs_samplesPerPixel = 200
    , cs_maxRecursionDepth = 50
    , cs_background = const (V3 0 0 0)
    , cs_vfov = degrees 40
    , cs_center = V3 278 278 (-800)
    , cs_lookAt = V3 278 278 0
    }

  in writeImageRTW "test_image.png" . raytrace settings world =<< newStdGen

main :: IO ()
main = do
  putStrLn "branch: monadic"
  let red = lambertian (constantTexture (V3 1 0 0))
  let world = red <$ sphere (V3 0 0 (-1)) 0.5
  let settings = defaultCameraSettings
  writeImage "test_image.png" . raytrace settings world =<< newStdGen