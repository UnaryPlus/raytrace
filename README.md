# raytrace

A ray tracing library.

Features:
* Spheres, parallelograms, and boxes (you can define your own shapes in addition to these)
* Volumes (fog and the like)
* A variety of materials, with behaviors including light emission and refraction
* Texture mapping
* Perlin noise for procedurally generated textures
* Parallel computation of pixels
* Bounding volume hierarchies
* Affine transformations (rotation, translation, ...)
* Optional defocusing to imitate a real camera

Possible future additions:
* Motion blur
* Triangular meshes
* ???

![Example](demo1.png)

The image above, with 405 million top-level rays, was generated on my laptop in about 8 minutes.