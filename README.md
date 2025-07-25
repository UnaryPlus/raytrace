# raytrace

A Haskell ray tracing library. Largely based on the books [Ray Tracing in One Weekend](https://raytracing.github.io/books/RayTracingInOneWeekend.html) and [Ray Tracing: The Next Week](https://raytracing.github.io/books/RayTracingTheNextWeek.html) by Peter Shirley.

Features:
* Spheres, parallelograms, and boxes (you can define your own shapes in addition to these)
* Volumes (fog and the like)
* A variety of materials, with behaviors including light emission and refraction
* Texture mapping
* Perlin noise for procedurally generated textures
* Parallel computation of pixels
* Bounding volume hierarchies
* Affine transformations (rotation, translation, and so on)
* Optional defocusing to simulate a real camera lens

Possible future additions:
* Motion blur
* Triangular meshes
* Less noise in scenes with small light sources

![Example](https://raw.githubusercontent.com/UnaryPlus/raytrace/refs/heads/main/demo1.png)

The image above, with 405 million top-level rays, was generated on my laptop in about 8 minutes. The blurriness in the foreground and background is due to defocusing, wherein only a single plane is in focus. The following image demonstrates texture mapping, light sources, and fog:

![Example](https://raw.githubusercontent.com/UnaryPlus/raytrace/refs/heads/main/demo2.png)

(The code for both of these images was based on code in the aforementioned books.)

## Example Usage

```haskell
module Main where

import Graphics.Ray
import Linear (V3(V3))
import System.Random (newStdGen)
import Data.Functor.Identity (Identity)

world :: Geometry Identity Material
world = group
  [ lambertian (checkerTexture 20 10 0.2 0.8) <$ sphere (V3 0 0 0) 1
  , lambertian (constantTexture (V3 0 0.2 0.5)) <$ sphere (V3 0 (-1000) 0) 999
  , mirror (constantTexture 0.8) <$ parallelogram (V3 (-3.25) (-1) (-0.75)) (V3 1.25 0 (-1.25)) (V3 0 2 0)
  ]

settings :: CameraSettings
settings = defaultCameraSettings
  { cs_center = V3 (-0.75) 0 2
  , cs_lookAt = V3 0 0 (-1)
  , cs_aspectRatio = 16 / 9
  , cs_imageWidth = 600
  , cs_samplesPerPixel = 50
  }

main :: IO ()
main = do
  seed <- newStdGen
  writeImage "example_image.png" (raytrace settings world seed)
```

This produces the following image:

![Example](https://raw.githubusercontent.com/UnaryPlus/raytrace/refs/heads/main/example_image.png)