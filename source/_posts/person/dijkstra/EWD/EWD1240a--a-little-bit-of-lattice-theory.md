# [EWD1240a] a little bit of lattice theory

- *type-constructor*
  (<lattice> = <lattice>)
  (<lattice> under <lattice>)

- *axiom*
  reflexive of under
  (x : <lattice> -> (x under x))

- the following proposition is equal to reflexive
  (x y : <lattice> (x = y) -> (x under y) (y under x))
  one-point-rule
  (x x : <lattice> (x = x) -> (x under x) (x under x))
  drop
  (x : <lattice> -> (x under x))

- *axiom*
  antisymmetric of under
  (x y : <lattice> (x under y) (y under x) -> (x = y))

- by reflexive of under
  we can proof indirect under
  1. ((x y z : <lattice> (z under x) -> (z under y)) -> (x under y))
  2. ((x y z : <lattice> (y under z) -> (x under z)) -> (x under y))
  proof 1
  (x y z : <lattice> (z under x) -> (z under y))
  instantiate
  (x y x : <lattice> (x under x) -> (x under y))
  reflexive of under
  (x y x : <lattice> true -> (x under y))
  drop
  (x y : <lattice> -> (x under y))
  end proof

- by antisymmetric of under
  we can proof indirect equal
  1. ((x y z : <lattice> (z under x) <-> (z under y)) -> (x = y))
  2. ((x y z : <lattice> (y under z) <-> (x under z)) -> (x = y))
  proof 1
  (x y z : <lattice> (z under x) <-> (z under y))
  instantiate 2 times
  (x y : <lattice>  ((x under x) <-> (x under y))  ((y under x) <-> (y under y)))
  reflexive of under
  (x y : <lattice>  (true <-> (x under y))  ((y under x) <-> true))
  drop
  (x y : <lattice>  (x under y)  (y under x))
  antisymmetric of under
  (x y : <lattice>  (x = y))
  end proof

- *axiom*
  the existence of w
  use under to define join [up] [lowest upper bound] [superemum]
  (x y : <lattice> -> w : <lattice> (z : <lattice> -> ((w under z) <-> (x under z) (y under z))))
  define
  (x y : <lattice> -> (x join y))
  or
  (x y : <lattice> -> w : (x join y))
  a new type constructor is defined
  need to proof w is unique
  to view this type constructor as function

- join
  1. idempotent
     (x join x) = x
  2. symmetry
     (x join y) = (y join x)
  3. associative
     ((x join y) join z) = (x join (y join z))
  proof associative
  ><><><
  end proof

- wish to proof
  (x y : <lattice> ((x join y) = y) <-> (x under y))
  and use it to proof the transitive of under
  (x y z : <lattice> (x under y) (y under z) -> (x under z))
  thus under is partial-order
  1. reflexive
  2. antisymmetric
  3. transitive
