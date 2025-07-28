{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Graphics.Ray

import Linear (V3(V3), (*^), normalize, norm, (!*!))
import System.Random (StdGen, newStdGen, randomR, random, mkStdGen)
import Control.Monad.State (State, runState, state)
import Control.Monad (forM, replicateM)
import Control.Applicative (liftA2)
import Data.Functor.Identity (Identity)

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
  writeImageSqrt "test_image.png" $ raytrace settings world seed

noiseTest :: IO ()
noiseTest = let
  groundMaterial = lambertian (noiseTexture 2 2.0 (V3 10 0 0) 0 1)
  ballMaterial = lambertian (marbleTexture (V3 0 0 1) 4 0)

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
  writeImageSqrt "noise_test.png" $ raytrace settings world seed

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
  writeImageSqrt "test_image.png" $ raytrace settings world seed

cuboidTest :: IO ()
cuboidTest = do
  globe <- readImage "images/earthmap.jpg"
  let globeMaterial = lambertian (imageTexture globe)
  let object = globeMaterial <$ cuboid (fromCorners (-V3 1 2 0.5) (V3 1 2 0.5))
  let world = transform (translate (V3 0 0 (-3)) !*! rotateX (degrees 60)) object
  let settings = defaultCameraSettings { cs_imageWidth = 300 }
  writeImage "test_image.png" . raytrace settings world =<< newStdGen

sphereUVTest :: IO ()
sphereUVTest = do
  globe <- readImage "images/earthmap.jpg"
  let globeMaterial = lambertian (imageTexture globe)
  let world = globeMaterial <$ group [ sphere (V3 0 0 (-2)) 0.4, sphere (V3 0 0 (-1)) 0.4 ]
  let settings = defaultCameraSettings { cs_imageWidth = 1, cs_samplesPerPixel = 1, cs_vfov = 0.0001}
  writeImage "test_image.png" (raytrace settings world (mkStdGen 12))

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

  genWorld :: State StdGen (Geometry Identity Material)
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
  writeImageSqrt "test_image.png" $ raytrace settings world seed'

cornellBox :: Int -> Int -> IO ()
cornellBox samplesPerPixel maxRecurionDepth = let
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
    , cs_samplesPerPixel = samplesPerPixel
    , cs_maxRecursionDepth = maxRecurionDepth
    , cs_background = const (V3 0 0 0)
    , cs_vfov = degrees 40
    , cs_center = V3 278 278 (-800)
    , cs_lookAt = V3 278 278 0
    , cs_redirectProb = 0.25
    , cs_redirectTarget = (,,) (V3 343 554 332) (V3 (-130) 0 0) (V3 0 0 (-105))
    }

  in writeImageSqrt "cornell_box.png" $ raytrace settings world (mkStdGen 234)

cornellSmoke :: IO ()
cornellSmoke = let
  red = lambertian (constantTexture (V3 0.65 0.05 0.05))
  white = lambertian (constantTexture (V3 0.73 0.73 0.73))
  green = lambertian (constantTexture (V3 0.12 0.45 0.15))
  light = lightSource (constantTexture (V3 7 7 7))

  surfaces = group
    [ green <$ parallelogram (V3 555 0 0) (V3 0 555 0) (V3 0 0 555)
    , red <$ parallelogram (V3 0 0 0) (V3 0 555 0) (V3 0 0 555)
    , light <$ parallelogram (V3 113 554 127) (V3 330 0 0) (V3 0 0 305)
    , white <$ parallelogram (V3 0 0 0) (V3 555 0 0) (V3 0 0 555)
    , white <$ parallelogram (V3 555 555 555) (V3 (-555) 0 0) (V3 0 0 (-555))
    , white <$ parallelogram (V3 0 0 555) (V3 555 0 0) (V3 0 555 0)
    ]
  
  cube1 = transform (translate (V3 265 0 295) !*! rotateY (degrees 15)) $ cuboid (fromCorners (V3 0 0 0) (V3 165 330 165))
  cube2 = transform (translate (V3 130 0 65) !*! rotateY (degrees (-18))) $ cuboid (fromCorners (V3 0 0 0) (V3 165 165 165))
  
  world = group
    [ pureGeometry surfaces
    , pitchBlack <$ constantMedium 0.01 cube1
    , isotropic (constantTexture 1) <$ constantMedium 0.01 cube2
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

  in writeImageSqrt "cornell_smoke.png" . raytrace settings world =<< newStdGen

demo2 :: FilePath -> Int -> Int -> Int -> IO ()
demo2 path imageWidth samplesPerPixel maxRecursionDepth = let
  ground = lambertian (constantTexture (V3 0.48 0.83 0.53))
  white = lambertian (constantTexture (V3 0.73 0.73 0.73))

  generateBoxes :: State StdGen (Geometry Identity Material)
  generateBoxes = 
    fmap ((ground <$) . bvhTree . autoTree) $ 
    forM (liftA2 (,) [0..19] [0..19]) $ \(i, j) -> do
      let x0 = -1000 + i * 100
      let z0 = -1000 + j * 100
      let x1 = x0 + 100
      let z1 = z0 + 100
      let y0 = 0
      y1 <- state (randomR (1, 101))
      pure (cuboid (fromCorners (V3 x0 y0 z0) (V3 x1 y1 z1)))
  
  generateBalls :: State StdGen (Geometry Identity Material)
  generateBalls =
    fmap ((white <$) . transform (translate (V3 (-100) 270 395) !*! rotateY (degrees 15)) . bvhTree . autoTree) $
    replicateM 1000 $ do
      p <- state (randomR (0, 165)) 
      pure (sphere p 10)
  
  boundary = sphere (V3 360 150 145) 70 
  light f = f (V3 123 554 147) (V3 300 0 0) (V3 0 0 265)
  
  largeObjects earth =
    [ lightSource (constantTexture (V3 7 7 7)) <$ light parallelogram
    , lambertian (constantTexture (V3 0.7 0.3 0.1)) <$ moving 0 (V3 30 0 0) (sphere (V3 400 400 200) 50)
    , dielectric 1.5 <$ sphere (V3 260 150 45) 50
    , dielectric 1.5 <$ boundary
    , metal 1.0 (constantTexture (V3 0.8 0.8 0.9)) <$ sphere (V3 0 150 145) 50
    , lambertian (imageTexture earth) <$ transform (translate (V3 400 0 400) !*! rotateY (pi/2)) (sphere (V3 0 200 0) 100)
    , lambertian (marbleTexture (V3 0 0 0.05) 4 0) <$ sphere (V3 220 280 300) 80
    ]
  
  generateWorld earth = do
    boxes <- generateBoxes
    balls <- generateBalls
    pure $ group 
      [ pureGeometry (group (boxes : balls : largeObjects earth))
      , isotropic (constantTexture 1) <$ constantMedium 0.0001 (sphere (V3 0 0 0) 5000)
      , isotropic (constantTexture (V3 0.2 0.4 0.9)) <$ constantMedium 0.2 boundary
      ]
  
  settings = defaultCameraSettings
    { cs_center = V3 478 278 (-600)
    , cs_lookAt = V3 278 278 0
    , cs_vfov = degrees 40
    , cs_aspectRatio = 1.0
    , cs_imageWidth = imageWidth
    , cs_samplesPerPixel = samplesPerPixel
    , cs_maxRecursionDepth = maxRecursionDepth
    , cs_background = const 0
    , cs_redirectProb = 0.25
    , cs_redirectTarget = light (,,) 
    }

  in do
    earth <- readImage "images/earthmap.jpg"
    seed <- newStdGen
    let (world, seed') = runState (generateWorld earth) seed
    writeImageSqrt path (raytrace settings world seed')

pawnTest :: IO ()
pawnTest = let
  world mesh = lambertian (checkerTexture 2 2 0.2 0.7) <$ bvhTree (autoTree (triangleMesh mesh))
  settings = defaultCameraSettings 
    { cs_center = V3 0 0.025 0.05
    , cs_imageWidth = 500
    , cs_samplesPerPixel = 200
    }
  in
  readObj "images/pawn.obj" >>= \case
    Left err -> putStrLn err
    Right mesh -> writeImage "test_image.png" (raytrace settings (world mesh) (mkStdGen 55))

-- This should take less than 110 seconds without redirection
cornellTest :: IO ()
cornellTest = cornellBox 200 50

-- This should take less than 70 seconds without redirection
demoTest :: IO ()
demoTest = demo2 "test_image.png" 400 250 4

main :: IO ()
main = pawnTest