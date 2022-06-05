---
title: Topology of 3-dimensional fibered spaces
subtitle: Topologie dreidi-mensionales gefaserter Raum
author: Herbert Seifert
year: 1933
---

# intro

- a way to to approach the 3-dimensional homeomorphism problem
  is to examine fundamental regions of groups
  acting on a 3-dimensional metric space.

- for example, in 2-dimension,
  a closed surface is the fundamental region of
  fundamental group acting on covering space of the closed surface.

  each such action has no fix point.

  the trace or orbit of an action is a discrete point set.

- for 3-sphere, we can observe a continuous group of spherical actions,
  the orbits of which are trace curves [circles],
  thus a fibration of the sphere [i.e. hopf fibration],
  the fundamental region of this group of actions is 2-sphere.

- instead of investigating a complete system of
  topological invariants of 3-dimensional manifolds,
  we search for a system of invariants
  for fiber preserving maps of fibered 3-manifolds.

  we can solve this problem,
  but two spaces with different fibrations can be homeomorphic,
  thus the homeomorphism problem remains open.

# fibered spaces

- with the definition of manifold [topological space]
  by the axioms of neighborhood system,
  [thus include manifold with boundaries]
  we can define fibered space as following :

  (5) The manifold can be decomposed into fibers,
  where each fiber is a simple closed curve [circle, maybe knoted].

  (6) Each point lies on exactly one fiber.

  (7) For each fiber H there exists a fiber neighborhood,
  that is, a subset consisting of fibers and containing H,
  which can be mapped under a fiber preserving map onto a fibered solid torus,
  where H is mapped onto the middle-fiber.

- A map is fiber preserving
  if it (1) is a homeomorphism
  and (2) maps fibers to fibers.

- x -
  since we want to use solid torus as neighborhood of circle
  to define fibered manifold,
  we investigate fibered solid torus first.

- a fibered solid torus can be classified by a rational number ((v / u) <= 1)
  which represent a twist
  [note that, the middle-fiber is not twisted.]

  thus we can add the following limitations
  (v and u are coprime) (u > 0) (0 <= v <= u/2)

  because twist over 1, or reverse the direction of the twist
  would give us fiber preserving homeomorphism from solid torus to itself.
  - such a homeomorphism would be an element of
    the fiber preserving mapping class group of the solid torus.

- The topological map of the solid torus,
  which sends a longitude to another which is not homologous (on T),
  [thus] cannot be obtained by a deformation of the identity.

  - x -
    what is meant by 'homologous of topological map of a space to itself'?
    a map as a group of chains?

- two fibered solid tori
  can be mapped onto each other by a fiber preserving map
  iff they have the same defining numbers u, v.

- basic example of fiber space is (S1 * S2)

- The boundary of an arbitrary fibered solid torus is a fibered torus.

  two fibered tori
  can be mapped ontoeach other under a fiber preserving homeomorphism.

  - x -
    how to understand is fact by subdivision of topological space?

  thus
  the boundaries of any two fibered solid tori
  can be mapped onto each other under a fiber preserving homeomorphism.

  - x -
    why fibered solid torus is classified by ((v / u) <= 1)
    while fibered torus are all fiber homeomorphic?
    because fibered solid torus have a middle-fiber?
    how to understand this fact by subdivision of topological space?

    maybe,
    torus fibered by circle can only have circle as base-space,
    while solid torus fibered by circle can have different base-spaces,
    when ((v / u) = (0 / 1)) the base-space is disk.
    when ((v / u) = (1 / 2)) the base-space is not disk anymore,
    because when cutting the solid torus by a disk,
    the disk must cross non-middle-fiber many times.

    is this how seifert manifold is discovered?

# orbit surface

- x -
  here is our base-space.

- The most important concept in the study of fibered spaces
  is that of the orbit surface.
  Every fibered space F has an orbit surface f.

  Now f is not a subset of the space F
  and can in general *not* be embedded in F,
  but is defined as follows :

  there is a one-to-one correspondence
  between the fibers of F and the points of f.
  - the orbit surface thus indicates
    how the manifold is "decomposed" into fibers.

- f is a manifold,
  this is ensured by 'fiber neighborhood of a fiber is a solid torus',
  for the meridian disk of the fiber neighborhood of a fiber H in F,
  will be a neighborhood of a point h in f.

- If F is closed,
  then f is an orientable surface of genus p (number of handles)
  or a nonorientable surface of genus k (number of cross-caps).

  if (F = (S1 * S2))
  then (f = S2)

  of course, for any surface f,
  we always can construct (F = (S1 * f))

- ><><><

- ordinary fiber -- (u = 1)
  exceptional fiber -- (u > 1)

  in the neighborhood of an exceptional fiber H of order u,
  If a fiber approaches H,
  its limit runs u times around H,
  and (u * H) is homologous to an ordinary fiber.

  In a fibered solid torus all the fibers are ordinary fibers,
  except possibly for the middle fiber.

  but the points of the orbit surface
  that are images of exceptional fibers are exceptional points;
  as points of the orbit surface,
  they cannot be distinguished from ordinary points.

  - then, when constructing the fiber-space
    by its base-space [orbit surface],
    how should we encode this information about exceptional points
    into the construction?

- Theorem 1.
  A closed fibered space
  contains at most finitely many exceptional fibers.

# fiberings of s3

- Before studying fiberings in general,
  we construct examples of fiberings of S3 with exceptional fibers.

# triangulations of fibered spaces

# drilling and filling (surgery)

# classes of fibered spaces

# the orientable fibered spaces

# the nonorientable fibered spaces

# covering spaces

# fundamental groups of fibered spaces

# fiberings of the 3-sphere (complete list)

# the fibered poincare spaces

# constructing poincare spaces from torus knots

# translation groups of fibered spaces

# spaces which cannot be fibered

# appendix: branched coverings
