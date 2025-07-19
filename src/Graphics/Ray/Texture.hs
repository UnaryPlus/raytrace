{-# LANGUAGE FlexibleContexts #-}
module Graphics.Ray.Texture 
  ( Texture(Texture)
  , constantTexture, solidTexture, uvTexture, imageTexture, checkerTexture, noiseTexture, marbleTexture
  ) where

import Graphics.Ray.Core
import Graphics.Ray.Noise

import Linear (V2(V2), V3, (^*), (*^), dot)
import qualified Data.Massiv.Array as A
import Data.Massiv.Array (Ix2((:.)), (!))
import Data.Bits ((.&.))

newtype Texture = Texture (Point3 -> V2 Double -> Color)

-- | Texture that is the same color everywhere.
constantTexture :: Color -> Texture
constantTexture color = Texture (\_ _ -> color)

-- | Texture assigning a color to each point in 3D space.
solidTexture :: (Point3 -> Color) -> Texture
solidTexture f = Texture (\p _ -> f p)

-- | Texture whose color depends on two parameters (u, v) that vary across a surface.
uvTexture :: (V2 Double -> Color) -> Texture
uvTexture f = Texture (const f)

-- | Convert an image into a texture. (u, v) = (0, 0) maps to the bottom left of the image,
-- (u, v) = (1, 1) maps to the top right, and coordinates outside of this range wrap around.
imageTexture :: (A.Manifest r Color) => A.Matrix r Color -> Texture
imageTexture image = let 
  A.Sz (h :. w) = A.size image 
  w' = fromIntegral w
  h' = fromIntegral h
  in
  uvTexture $ \(V2 u v) -> let
    i = floor (u * w') `mod` w
    j = floor ((1 - v) * h') `mod` h
    in
    image ! (j :. i)

-- | UV texture with two colors alternating in a checkerboard pattern. The first two arguments
-- are the dimensions of the checkerboard.
checkerTexture :: Int -> Int -> Color -> Color -> Texture
checkerTexture n_u n_v c0 c1 = let
  n_u' = fromIntegral n_u
  n_v' = fromIntegral n_v
  in
  uvTexture $ \(V2 u v) -> let
    i = floor (u * n_u')
    j = floor (v * n_v')
    in if (i + j) .&. 1 == (0 :: Int) then c0 else c1

-- | Perlin noise texture.
noiseTexture 
  :: Int -- ^ Number of layers of noise (see 'fractalNoise')
  -> Double -- ^ Noise frequency
  -> V3 Double -- ^ Shift applied before calling noise function
  -> Color -- ^ Color 1
  -> Color -- ^ Color 2
  -> Texture
noiseTexture k freq shift color0 color1 = let
  scale = 0.5 / 0.8
  getNoise p = fractalNoise k (p ^* freq + shift) * scale + 0.5 
  diff = color1 - color0
  in solidTexture $ \p -> color0 + diff ^* getNoise p

-- | Texture with noisy black and white stripes, resulting in a marble-like appearance. 
marbleTexture 
  :: Vec3 -- ^ Direction of stripes
  -> Double -- ^ Frequency
  -> V3 Double -- ^ Shift applied before calling noise function
  -> Texture 
marbleTexture dir freq shift =
  solidTexture $ \p -> let
    sinArg = freq * dot dir p
    noise = 10 * turbulence 7 (0.25 * freq *^ p + shift)
    in 1 ^* (0.5 + 0.5 * sin (sinArg + noise))