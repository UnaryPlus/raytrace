{-# LANGUAGE FlexibleContexts #-}
module Graphics.Ray.Texture 
  ( Texture(Texture)
  , constantTexture, solidTexture, uvTexture, checkerTexture, noiseTexture, marbleTexture, imageTexture
  ) where

import Graphics.Ray.Core
import Graphics.Ray.Noise

import Linear (V2(V2), V3(V3), (^*))
import qualified Data.Massiv.Array as A
import Data.Massiv.Array (Ix2((:.)), (!))

newtype Texture = Texture (Point3 -> V2 Double -> Color)

constantTexture :: Color -> Texture
constantTexture color = Texture (\_ _ -> color)

solidTexture :: (Point3 -> Color) -> Texture
solidTexture f = Texture (\p _ -> f p)

uvTexture :: (V2 Double -> Color) -> Texture
uvTexture f = Texture (const f)

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

checkerTexture :: Double -> Color -> Color -> Texture
checkerTexture scale c1 c2 =
  let invScale = 1 / scale in
  solidTexture $ \p ->
    let ns = fmap floor (p ^* invScale) :: V3 Int in
    if even (sum ns) then c1 else c2

noiseTexture :: Int -> Double -> V3 Double -> Color -> Color -> Texture
noiseTexture k freq shift color0 color1 = let
  invSqrt3 = 1 / sqrt 3
  getNoise p = fractalNoise k (p ^* freq + shift) * invSqrt3 + 0.5 
  diff = color1 - color0
  in solidTexture $ \p -> color0 + diff ^* getNoise p

marbleTexture :: Double -> Texture
marbleTexture freq =
  solidTexture $ \p@(V3 _ _ z) ->
    1 ^* (0.5 + 0.5 * sin (z * freq + 10 * turbulence 7 p))