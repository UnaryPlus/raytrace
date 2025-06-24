{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Graphics.Ray.SceneObject where

import Graphics.Ray.Core
import Graphics.Ray.Geometry
import Graphics.Ray.Material

import Control.Applicative ((<|>))
import Data.List (sortOn)

data SceneObject = SceneObject Box (Ray -> Interval -> Maybe (HitRecord, Material))

boundingBox :: SceneObject -> Box
boundingBox (SceneObject b _) = b

-- TODO: change name
geometryObject :: Material -> Geometry -> SceneObject
geometryObject mat (Geometry bbox f) = 
  SceneObject bbox (\r i -> fmap (, mat) (f r i))

group :: [SceneObject] -> SceneObject
group obs = let
  bbox = boxHull (map boundingBox obs)
  
  hitGroup ray (tmin, tmax) =
    let try (tmax', knownHit) (SceneObject _ hitObj) =
          case hitObj ray (tmin, tmax') of
            Nothing -> (tmax', knownHit)
            Just (hit, mat) -> (hr_t hit, Just (hit, mat))
    in snd (foldl try (tmax, Nothing) obs)
  
  in SceneObject bbox hitGroup

bvhNode :: SceneObject -> SceneObject -> SceneObject
bvhNode (SceneObject bboxLeft hitLeft) (SceneObject bboxRight hitRight) = let
  bbox = boxJoin bboxLeft bboxRight

  hitBvhNode ray (tmin, tmax)
    | hitsBox bbox ray (tmin, tmax) = 
      case hitLeft ray (tmin, tmax) of
        Nothing -> hitRight ray (tmin, tmax)
        res@(Just (hit, _)) -> hitRight ray (tmin, hr_t hit) <|> res
    | otherwise = Nothing
  
  in SceneObject bbox hitBvhNode

data Tree a = Leaf a | Node (Tree a) (Tree a)

bvhTree :: Tree SceneObject -> SceneObject
bvhTree = \case
  Leaf a -> a
  Node left right -> bvhNode (bvhTree left) (bvhTree right)

autoTree :: [SceneObject] -> Tree SceneObject
autoTree = \case
  [] -> error "autoTree: empty list"
  [obj] -> Leaf obj
  obs -> let
    d = longestDim (boxHull (map boundingBox obs))
    obs' = sortOn (midpoint . component d . boundingBox) obs
    (left, right) = splitAt (length obs `div` 2) obs'
    in Node (autoTree left) (autoTree right)
  

