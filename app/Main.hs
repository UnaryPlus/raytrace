{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Graphics.Ray.Core
import Graphics.Ray.Material
import Graphics.Ray.SceneObject
import Graphics.Ray.Texture
import Graphics.Ray.Geometry
import Graphics.Ray

import Linear (V3(V3), (*^), normalize, norm)
import System.Random (StdGen, newStdGen, randomR, random)
import Control.Monad.State (State, runState, state)
import Control.Monad (forM)
import Control.Applicative (liftA2)

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
    [ geometryObject materialGround (sphere (V3 0 (-100.5) (-1)) 100)
    , geometryObject materialCenter (sphere (V3 0 0 (-1.2)) 0.5) 
    , geometryObject materialLeft (sphere (V3 (-1) 0 (-1)) 0.5) 
    , geometryObject materialBubble (sphere (V3 (-1) 0 (-1)) 0.4) 
    , geometryObject materialRight (sphere (V3 1 0 (-1)) 0.5) 
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
    [ geometryObject checker (sphere (V3 0 (-10) 0) 10)
    , geometryObject checker (sphere (V3 0 10 0) 10)
    ]
  
  settings = defaultCameraSettings
    { cs_aspectRatio = 16 / 9
    , cs_imageWidth = 400
    , cs_samplesPerPixel = 100
    , cs_maxRecursionDepth = 50
    , cs_vfov = degrees 20
    , cs_center = V3 13 2 3
    , cs_lookAt = V3 0 0 0
    }

  in do
  seed <- newStdGen
  writeImageRTW "test_image.png" $ raytrace settings world seed

demo1 :: IO ()
demo1 = let
  materialGround = lambertian (constantTexture (V3 0.5 0.5 0.5))
  materialGlass = dielectric 1.5
  materialDiffuse = lambertian (constantTexture (V3 0.4 0.2 0.1))
  materialMirror = mirror (constantTexture (V3 0.7 0.6 0.5))

  bigSpheres =
    [ geometryObject materialGround (sphere (V3 0 (-1000) 0) 1000)
    , geometryObject materialGlass (sphere (V3 0 1 0) 1)
    , geometryObject materialDiffuse (sphere (V3 (-4) 1 0) 1)
    , geometryObject materialMirror (sphere (V3 4 1 0) 1)
    ]

  genWorld :: State StdGen SceneObject
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
        pure [ geometryObject mat (sphere center 0.2) ]
  
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

main :: IO ()
main = demo1