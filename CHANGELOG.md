# Revision history for raytrace

## 0.1.0.0 &mdash; 2025-07-18

* First version. Released on an unsuspecting world.

## 0.2.0.0 &mdash; 2025-07-31

* New material model allowing ray redirection
* Added camera setting `cs_redirectTargets`
* Added triangles and meshes
* Added `planeShape` function
* Added time parameter to `Geometry`, allowing for motion blur
* Different type signatures for `boxJoin`, `boxHull`, and `bvhTree`
* Removed `autoTree` function
* New materials: `anisotropic` and `lommelSeeliger`
* Volumes can be non-convex
* All rays are normalized