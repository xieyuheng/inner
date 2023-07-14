---
title: The Topology of CW Complexes
authors: [Albert T. Lundell, Stephen Weingram]
year: 1969
---

# combinatorial cell complexes

## 1 definitions

- n-cell can be viewed as n-disk or n-cube or n-simplex.
  boundary of n-cell can be viewed as (n-1)-sphere,
  or two (n-1)-disk glued together etc.

- the cell-structure of a space X,
  is depicted by a family of attaching-maps
  [characteristic-map] [adjunction]
  of type `n-cell -> X`

- the three axioms of cell-structure
  are expressed in the language of set theory,
  (1) an attaching-map is injective on the inner part of the cell.
  (2) the images of all inner parts of the attaching-maps
  --- are disjoint and cover X.
  (3) an attaching-map attaches the boundary of n-cell
  --- to the images of lower-cells.

- note that,
  the axiom (3) in the book is that

  - the image of the boundary of n-cell under the attaching-map
    is a sub-set of the lower-cells.

  this is weaker than how AT1 is,
  in AT1 the image of the boundary is always a subcomplexes.

- we use the concept of cell-structure,
  instead of viewing cell-complex as gluing cells together,
  because we want to define the product-space of two cell-complex
  by say how we can view it as a new cell-complex,
  or say, by revealing its cell-structure.

- the axioms can be interpreted in AT1 as following
  (1) and (2) space X is constructed by gluing all the cells.
  (3) cells are glued together by common boundary.

- in AT1, cell is simply 'cell',
  we do not distinguish open or closed cell.

- the mapping-class-group of n-cell are all trivial,
  no matter with the n-cell's boundary fixed or not.

  thus, for an attaching-map, after fixing its image in X,
  under homotopy equivalence, there is only one way
  to map the n-cell to this image.

  this is why we glue cells together,
  instead of gluing tori or other continuums together.

- when defining cell-structure in the language of set theory,
  we have to use the 'closure-finite' condition to limit the definition
  to our intuition of 'cells glued together'.

- 'closure-finite' is defined as,
  each n-cell meets only a finite number of lower level open cells.

  in AT1, this is always true,
  because the boundary of a cell
  is always attached only to finite number of lower cells.

- 'closure-finite' is the 'C' of CW-complex,
  while the 'W' denotes weak topology,
  which is used to define the topological structure of cell-complex.

- in AT1,
  a regular-cell is the cell whose boundary has no self common parts,
  and the boundary is not level-down-ed by 'refl'.

- there is also the concept of a 'normal-cell',
  which is defined as cells that are also subcomplexes,
  while in AT1, a cell can not fail to be subcomplexes.

## 2 examples

- the pathologies occurs in the language of set theory,
  does not occur in AT1.
  because for us the axiom (3) is stronger.

## 3 carrier theory

- the carrier topology of a cell complex
  is to view subcomplexes as closed sets of the topology.

## 4 functions

## 5 product complexes

## 6 equivalence relations and quotients

## 7 adjunction complexes

# cw complexes

## 1 definitions

- the term 'CW-complex' means to make clear
  the topological structure of cell-complex.
  which is defined by the weak topology (initial topology)
  with respect to the family of characteristic-maps.

- under such topology, a map is continuous iff
  it is continuous on each cells.

# regular and semisimplicial cw complexes

# homotopy type of cw complexes

# the singular homology of cw complexes
